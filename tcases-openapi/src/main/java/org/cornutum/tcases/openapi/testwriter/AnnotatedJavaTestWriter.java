//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import org.cornutum.tcases.io.IndentedWriter;
import org.cornutum.tcases.openapi.resolver.RequestCase;
import static org.cornutum.tcases.DefUtils.toNumberIdentifiers;
import static org.cornutum.tcases.util.CollectionUtils.fromCsv;

import static org.apache.commons.io.FilenameUtils.getBaseName;
import static org.apache.commons.io.FilenameUtils.getExtension;
import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.apache.commons.lang3.StringUtils.removeEnd;
import static org.apache.commons.lang3.text.WordUtils.capitalize;

import java.io.File;
import java.util.Arrays;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import static java.util.stream.Collectors.joining;

/**
 * Base class for {@link JavaTestWriter} implementations that identify test methods via annotations.
 */
public abstract class AnnotatedJavaTestWriter extends JavaTestWriter
  {
  /**
   * Creates a new AnnotatedJavaTestWriter instance.
   */
  protected AnnotatedJavaTestWriter( TestCaseWriter testCaseWriter)
    {
    super( testCaseWriter);
    }

  /**
   * Writes the target test annotation dependencies to the given stream.
   */
  protected abstract void writeTestAnnotationDependencies( JavaTestTarget target, String testName, IndentedWriter targetWriter);

  /**
   * Writes the annotation for a target test case to the given stream.
   */
  protected abstract void writeTestAnnotation( JavaTestTarget target, String testName, RequestCase requestCase, IndentedWriter targetWriter);

  /**
   * Writes the target test dependencies to the given stream.
   */
  @Override
protected void writeDependencies( JavaTestTarget target, String testName, IndentedWriter targetWriter)
    {
    super.writeDependencies( target, testName, targetWriter);

    targetWriter.println();
    writeTestAnnotationDependencies( target, testName, targetWriter);
    }

  /**
   * Writes a target test case to the given stream.
   */
  @Override
protected void writeTestCase( JavaTestTarget target, String testName, RequestCase requestCase, IndentedWriter targetWriter)
    {
    targetWriter.println();
    writeTestAnnotation( target, testName, requestCase, targetWriter);
    targetWriter.println( String.format( "public void %s() {", getMethodName( requestCase)));
    targetWriter.indent();
    
    super.writeTestCase( target, testName, requestCase, targetWriter);

    targetWriter.unindent();
    targetWriter.println( "}");
    }

  /**
   * Returns the target file defined by the given target.
   */
  @Override
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
  @Override
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
          methodName.append( toIdentifier( segmentMatcher.group()));
          }
        });

    // ... followed by the request case description
    getDescriptor( requestCase)
      .ifPresent( descriptor -> methodName.append( "_").append( descriptor));
    
    return methodName.toString();
    }

  /**
   * Returns an identifier containing a description of the given request case.
   * Returns <CODE>Optional.empty()</CODE> if no description is needed.
   */
  protected Optional<String> getDescriptor( RequestCase requestCase)
    {
    return
      "None.Defined='No'".equals( requestCase.getName())
      ? Optional.empty()
      : Optional.of( createDescriptor( requestCase));
    }

  /**
   * Returns an identifier containing a description of the given request case.
   */
  protected String createDescriptor( RequestCase requestCase)
    {
    return
      Optional.ofNullable( requestCase.getName())
      .map( name -> getBindingsDescriptor( name).orElse( toIdentifier( name)))
      .orElse( String.valueOf( requestCase.getId()));
    }

  /**
   * If the given text describes a set of variable bindings, returns a description of the bindings.
   * Otherwise, returns <CODE>Optional.empty()</CODE>.
   */
  protected Optional<String> getBindingsDescriptor( String text)
    {
    Stream.Builder<String> bindings = Stream.builder();
    Matcher varBindingMatcher = varBindingPattern_.matcher( text);
    while( varBindingMatcher.find())
      {
      String varId = toIdentifier( removeEnd( varBindingMatcher.group(1), ".Is"));

      String[] value = fromCsv( varBindingMatcher.group(2)).toArray( String[]::new);
      String valueId =
        value.length == 0?
        "Empty" :
        
        value[0] == null?
        "Null" :

        isBlank( value[0])?
        "Blank" :

        toIdentifier(
          toNumberIdentifiers( value[0])
          .replaceAll( " *<= *", "Leq_")
          .replaceAll( " *< *", "Lt_")
          .replaceAll( " *>= *", "Geq_")
          .replaceAll( " *> *", "Gt_"));

      bindings.add( String.format( "%s_Is_%s", varId, valueId));
      }

    String descriptor = bindings.build().collect( joining( "_"));
    
    return
      descriptor.isEmpty()
      ? Optional.empty()
      : Optional.of( descriptor);
    }

  /**
   * Reduces the given text to a single identifier.
   */
  protected String toIdentifier( String text)
    {
    return
      Arrays.stream( text.trim().split( "\\W+"))
      .map( id -> capitalize( id))
      .collect( joining( ""));
    }

  private static final Pattern uriSegmentPattern_ = Pattern.compile( "([^{}]+)|\\{([^}]+)\\}");
  private static final Pattern varBindingPattern_ = Pattern.compile( "([\\w\\-.]+)=([^\\&]+)");
  }
