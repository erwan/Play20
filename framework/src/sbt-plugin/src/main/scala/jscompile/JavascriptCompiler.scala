package play.core.jscompile

import sbt.PlayExceptions.AssetCompilationException
import java.io._
import play.api._
import scala.collection.JavaConverters._
import scalax.file._
import com.google.javascript.rhino.Node
import com.google.javascript.jscomp.ProcessCommonJSModules
import scala.io.Source

object JavascriptCompiler {

  import com.google.javascript.jscomp.{ Compiler, CompilerOptions, JSSourceFile }

  lazy val compiler = new Compiler()
  lazy val extern = JSSourceFile.fromCode("externs.js", "function alert(x) {}")

  /**
   * Compile a JS file with its dependencies
   * @return a triple containing the unminifed source code, the minified source code, the list of dependencies (including the input file)
   */
  def compile(source: File): (String, Option[String], Seq[File]) = {

    val debug = resolveAndCompile(source, false)
    val minified = resolveAndCompile(source, true)

    (debug, Some(minified), Seq(source))
  }

  def resolveAndCompile(source: File, minify: Boolean) = {
    val options = new CompilerOptions()
    options.prettyPrint = !minify
    options.setProcessCommonJSModules(true)
    options.setCommonJSModulePathPrefix(source.getParent() + "/")
    options.setManageClosureDependencies(Seq(toModuleName(source.getName())).asJava)

    val all = allSiblings(source)
    val input = (baseJs +: all.map(f => JSSourceFile.fromFile(f))).toArray

    compiler.compile(extern, input, options).success match {
      case true => compiler.toSource()
      case false => {
        val error = compiler.getErrors().head
        throw AssetCompilationException(Some(source), error.description, error.lineNumber, 0)
      }
    }
  }

  def allSiblings(source: File):Seq[File] = allJsFilesIn(source.getParentFile())

  def allJsFilesIn(dir: File): Seq[File] = dir.listFiles(new FileFilter {
    override def accept(f: File) = f.getName().endsWith(".js")
  })

  def baseJs = JSSourceFile.fromCode("base.js", Source.fromURL(getClass.getResource("/jscompiler/base.js")).mkString)

  /**
   * Minify a Javascript string
   */
  def minify(source: String, name: Option[String]): String = {

    val options = new CompilerOptions()

    val input = JSSourceFile.fromCode(name.getOrElse("unknown"), source)

    compiler.compile(extern, input, options).success match {
      case true => compiler.toSource()
      case false => {
        val error = compiler.getErrors().head
        throw AssetCompilationException(None, error.description, error.lineNumber, 0)
      }
    }

  }

  /**
   * Turns a filename into a JS identifier that is used for moduleNames in
   * rewritten code. Removes leading ./, replaces / with $, removes trailing .js
   * and replaces - with _. All moduleNames get a "module$" prefix.
   */
  def toModuleName(filename: String) = {
    "module$" + filename.replaceAll("^\\./", "").replaceAll("/", "\\$").replaceAll("\\.js$", "").replaceAll("-", "_");
  }

}


