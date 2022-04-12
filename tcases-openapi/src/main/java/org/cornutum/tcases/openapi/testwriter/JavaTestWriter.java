//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import org.cornutum.tcases.io.IndentedWriter;

import static org.apache.commons.io.FilenameUtils.getBaseName;
import static org.apache.commons.io.FilenameUtils.getExtension;
import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.apache.commons.lang3.text.WordUtils.capitalize;
import static org.apache.commons.lang3.text.WordUtils.capitalizeFully;

import java.io.File;
import java.util.List;
import java.util.Optional;
import java.util.stream.IntStream;
import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.joining;

/**
 * Writes Java source code for a test that executes API requests.
 */
public abstract class JavaTestWriter extends TestWriter<TestSource,JavaTestTarget>
  {
  /**
   * Creates a new JavaTestWriter instance.
   */
  protected JavaTestWriter( TestCaseWriter testCaseWriter)
    {
    super( testCaseWriter);
    }

  /**
   * Returns the test name derived from the given base name.
   */
  @Override
  protected String getTestName( String baseName)
    {
    String[] words = baseName.split( "\\W+");

    return
      IntStream.range( 0, words.length)
      .mapToObj( i -> i==0? capitalize( words[i]) : capitalizeFully( words[i]))
      .collect( joining( ""));
    }

  /**
   * Returns the test class name derived from the given test name.
   */
  protected abstract String getClassName( String testName);

  /**
   * Writes the target test opening to the given stream.
   */
  @Override
  protected void writeOpening( JavaTestTarget target, String testName, IndentedWriter targetWriter)
    {
    targetWriter.println(
      String.format(
        "package %s;",
        Optional.ofNullable( target.getTargetPackage())
        .orElseThrow( () -> new TestWriterException( String.format( "No package defined for target=%s", target)))));
    }

  /**
   * Writes the target test dependencies to the given stream.
   */
  @Override
  protected void writeDependencies( JavaTestTarget target, String testName, IndentedWriter targetWriter)
    {
    targetWriter.println();
    
    Optional.ofNullable( target.getBaseClassPackage())
      .filter( basePkg -> !basePkg.equals( target.getTargetPackage()))
      .ifPresent( basePkg -> targetWriter.println( String.format( "import %s", target.getBaseClass())));

    if( target.validateResponses())
      {
      targetWriter.println();
      targetWriter.println( "import org.cornutum.tcases.openapi.test.ResponseValidator;");
      }
    }

  /**
   * Writes the target test declarations to the given stream.
   */
  @Override
  protected void writeDeclarations( JavaTestTarget target, String testName, IndentedWriter targetWriter)
    {
    StringBuilder classDecl = new StringBuilder() .append( "public class ") .append( getClassName( testName));

    Optional.ofNullable( target.getBaseClassName())
      .ifPresent( baseClass -> classDecl.append( " extends ").append( baseClass));

    classDecl.append( " {");
    
    targetWriter.println();
    targetWriter.println( classDecl.toString());
    targetWriter.indent();
    
    if( target.validateResponses())
      {
      targetWriter.println();
      targetWriter.println( "private ResponseValidator responseValidator = new ResponseValidator( getClass());");
      }
    }

  /**
   * Writes the target test closing to the given stream.
   */
  @Override
  protected void writeClosing( JavaTestTarget target, String testName, IndentedWriter targetWriter)
    {
    targetWriter.unindent();
    targetWriter.println( "}");
    }

  /**
   * Returns the resource directory derived from the given target file and resource directory options.
   */
  @Override
  protected File getTestResourceDir( File targetFile, File resourceDir)
    {
    return
      super.getTestResourceDir(
        targetFile,

        Optional.ofNullable( resourceDir)
        .orElseGet( () -> getMavenResourceDir( targetFile).orElse( null)));
    }

  /**
   * Returns the target file defined by the given target.
   */
  @Override
  protected File getTargetFile( JavaTestTarget target, String testName)
    {
    File targetFile = super.getTargetFile( target, testName);

    return
      targetFile != null && isBlank( getExtension( targetFile.getName()))
      ? new File( targetFile.getParentFile(), String.format( "%s.java", getBaseName( targetFile.getName())))
      : targetFile;
    }

  /**
   * If the given Java file belongs to a Maven project, returns its associated resource directory.
   */
  private Optional<File> getMavenResourceDir( File javaFile)
    {
    List<String> dirs =
      Optional.ofNullable( javaFile)
      .map( file -> TestTarget.getPathElements( file.getParentFile()))
      .orElse( emptyList());

    return
      Optional.of( dirs.indexOf( "java"))
      .filter( i -> i >= 0)
      .map( i -> {

        String resourceDirRoot =
          Optional.of( dirs.subList( 0, i).stream().collect( joining( "/")))
          .filter( root -> !root.isEmpty())
          .map( root -> String.format( "%s/", root))
          .orElse( "");

        String resourceDirRelative = dirs.subList( i+1, dirs.size()).stream().collect( joining( "/"));

        return new File( String.format( "%sresources/%s", resourceDirRoot, resourceDirRelative));
        });
    }
  }
