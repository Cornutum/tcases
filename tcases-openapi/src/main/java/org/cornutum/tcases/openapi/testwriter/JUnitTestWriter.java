//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import org.cornutum.tcases.io.IndentedWriter;
import org.cornutum.tcases.openapi.resolver.RequestCase;

import static org.apache.commons.io.FilenameUtils.getBaseName;
import static org.apache.commons.io.FilenameUtils.getExtension;
import static org.apache.commons.lang3.text.WordUtils.capitalize;

import java.io.File;
import java.util.Arrays;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Writes Java source code for a JUnit test that executes API requests.
 */
public class JUnitTestWriter extends JavaTestWriter
  {
  /**
   * Creates a new JUnitTestWriter instance.
   */
  public JUnitTestWriter( TestCaseWriter testCaseWriter)
    {
    super( testCaseWriter);
    }

  /**
   * Writes the target test dependencies to the given stream.
   */
  protected void writeDependencies( JavaTestTarget target, String testName, IndentedWriter targetWriter)
    {
    super.writeDependencies( target, testName, targetWriter);

    targetWriter.println();
    targetWriter.println( "import org.junit.Test;");
    }

  /**
   * Writes a target test case to the given stream.
   */
  protected void writeTestCase( JavaTestTarget target, String testName, RequestCase requestCase, IndentedWriter targetWriter)
    {
    targetWriter.println();
    targetWriter.println( "@Test");
    targetWriter.println( String.format( "public void %s() {", getMethodName( requestCase)));
    targetWriter.indent();
    
    super.writeTestCase( target, testName, requestCase, targetWriter);

    targetWriter.unindent();
    targetWriter.println( "}");
    }

  /**
   * Returns the target file defined by the given target.
   */
  protected File getTargetFile( JavaTestTarget target, String testName)
    {
    File targetFile = super.getTargetFile( target, testName);

    return
      targetFile != null && "java".equals( getExtension( targetFile.getName()))
      ? new File( targetFile.getParentFile(), String.format( "%s.java", getClassName( getBaseName( targetFile.getName()))))
      : targetFile;
    }

  /**
   * Returns the test class name derived from the given test name.
   */
  protected String getClassName( String testName)
    {
    boolean isStandardTestClass =
      testName.startsWith( "Test")
      || testName.endsWith( "Test")
      || testName.endsWith( "Tests")
      || testName.endsWith( "TestCase");

    return
      isStandardTestClass
      ? testName
      : testName + "Test";
    }

  /**
   * Returns a test method name for the given request case.
   */
  protected String getMethodName( RequestCase requestCase)
    {
    StringBuilder methodName = new StringBuilder();

    // The request operation...
    methodName.append( requestCase.getOperation().toLowerCase());

    // ... followed by...
    Arrays.stream( requestCase.getPath().split( "/"))
      // ... for each segment of the request path...
      .forEach( segment -> {
        Matcher segmentMatcher = uriSegmentPattern_.matcher( segment);
        while( segmentMatcher.find())
          {
          // ... the sequence of identifiers it contains...
          Arrays.stream( segmentMatcher.group().trim().split( "\\W+"))
            .forEach( name -> methodName.append( capitalize( name)));
          }
        });

    // ... followed by the request case id
    methodName.append( "_").append( requestCase.getId());
    
    return methodName.toString();
    }

  private static final Pattern uriSegmentPattern_ = Pattern.compile( "([^{}]+)|\\{([^}]+)\\}");  
  }
