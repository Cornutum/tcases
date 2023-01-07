//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.*;
import org.cornutum.tcases.util.ConditionRecorder;

import org.junit.Before;
import org.leadpony.justify.api.JsonValidatingException;
import org.leadpony.justify.api.Problem;
import static org.cornutum.hamcrest.Composites.*;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.Arrays;
import java.util.List;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.toList;

/**
 * Base class for tests of {@link SystemInputJsonWriter} and {@link SystemInputJsonReader}.
 */
public abstract class SystemInputJsonTest
  {
  @Before
  public void setUpReader()
    {
    systemInputResources_.setJsonReaderProducer(
      stream -> {
      SystemInputJsonReader reader = new SystemInputJsonReader( stream);
      reader.setNotifier( getConditionRecorder());
      return reader;
      });
    }
  
  public SystemInputDef testSystemInputResource( String systemInputResource)
    {
    return testSystemInput( systemInputResource, systemInputResources_.read( systemInputResource));
    }
  
  public SystemInputDef testSystemInputJsonResource( String systemInputResource)
    {
    return testSystemInput( systemInputResource, systemInputResources_.readJson( systemInputResource));
    }
  
  public SystemInputDef testSystemInput( String systemInputResource, SystemInputDef systemInputBefore)
    {
    // When...
    ByteArrayOutputStream systemInputOut = new ByteArrayOutputStream();
    try( SystemInputJsonWriter writer = new SystemInputJsonWriter( systemInputOut))
      {
      writer.write( systemInputBefore);
      }

    SystemInputDef systemInputAfter;
    ByteArrayInputStream systemInputIn = new ByteArrayInputStream( systemInputOut.toByteArray());
    try( SystemInputJsonReader reader = new SystemInputJsonReader( systemInputIn))
      {
      systemInputAfter = reader.getSystemInputDef();
      }    

    // Then...
    assertThat( "Output from definition=" + systemInputResource, systemInputAfter, matches( new SystemInputDefMatcher( systemInputBefore)));
    assertThat( "Copy of definition=" + systemInputResource, SystemInputDefBuilder.with( systemInputAfter).build(), matches( new SystemInputDefMatcher( systemInputBefore)));

    return systemInputAfter;
    }

  public void expectSystemInputJson( String systemInputResource, SystemInputDef inputDef)
    {
    // When...
    SystemInputDef inputDefAfter = testSystemInputJsonResource( systemInputResource);

    // Then...
    assertThat( systemInputResource, inputDefAfter, matches( new SystemInputDefMatcher( SystemInputs.normalized( inputDef))));
    assertThat( "Copy of " + systemInputResource, SystemInputDefBuilder.with( inputDef).build(), matches( new SystemInputDefMatcher( inputDef)));
    }
  
  public void assertDefinitionError( String systemInputResource, String... expected)
    {
    expectFailure( SystemInputException.class)
      .when( () -> systemInputResources_.readJson( systemInputResource), t -> t.getCause())
      .then( failure -> {
        Stream.Builder<String> causes = Stream.builder();
        for( Throwable cause = failure; cause != null; cause = cause.getCause())
          {
          causes.add( cause.getMessage());
          }
          
        assertThat( "Causes", causes.build().collect( toList()), listsMembers( expected));
        });
    }

  public void assertValidationFailure( String systemInputResource, String... expected)
    {
    expectFailure( SystemInputException.class)
      .when( () -> systemInputResources_.readJson( systemInputResource), t -> t.getCause())
      .then( failure -> {
        Throwable cause = failure.getCause();
        assertThat( "Cause", cause.getClass(), equalTo( JsonValidatingException.class));

        JsonValidatingException jve = (JsonValidatingException)cause;
        List<String> problems = problems( jve).map( p -> p.getMessage()).distinct().collect( toList());
        assertThat( "Problems", problems, containsInAnyOrder( Arrays.stream( expected).map( m -> containsString(m)).collect( toList())));
        });
    }
  
  protected void assertWarnings( String... warnings)
    {
    assertThat( "Warnings", getConditionRecorder().getWarnings(), listsMembers( warnings));
    assertThat( "Errors", getConditionRecorder().getErrors(), listsMembers( emptyList()));
    }

  protected void assertErrors( String... errors)
    {
    assertThat( "Errors", getConditionRecorder().getErrors(), listsMembers( errors));
    assertThat( "Warnings", getConditionRecorder().getWarnings(), listsMembers( emptyList()));
    }
  
  protected void assertConditions( List<String> warnings, List<String> errors)
    {
    assertThat( "Warnings", getConditionRecorder().getWarnings(), listsMembers( warnings));
    assertThat( "Errors", getConditionRecorder().getErrors(), listsMembers( errors));
    }
  
  protected void assertConditionsNone()
    {
    assertConditions( emptyList(), emptyList());
    }

  protected ConditionRecorder getConditionRecorder()
    {
    return conditionRecorder_;
    }

  private Stream<Problem> problems( JsonValidatingException jve)
    {
    return jve.getProblems().stream().flatMap( p -> problems( p));
    }

  private Stream<Problem> problems( Problem problem)
    {
    return
      problem.hasBranches()?
      
      IntStream.range( 0, problem.countBranches())
      .mapToObj( i -> problem.getBranch(i))
      .flatMap( problems -> problems.stream().flatMap( p -> problems( p))) :

      Stream.of( problem);
    }

  protected SystemInputResources systemInputResources_ = new SystemInputResources( SystemInputJsonTest.class);
  protected ConditionRecorder conditionRecorder_ = new ConditionRecorder();
  }
