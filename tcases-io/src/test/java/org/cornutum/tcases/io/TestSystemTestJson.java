//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.*;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;
import static org.cornutum.hamcrest.Composites.*;

import org.junit.Test;
import org.leadpony.justify.api.JsonValidatingException;
import org.leadpony.justify.api.Problem;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.Arrays;
import java.util.List;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toList;

/**
 * Runs tests for the {@link SystemTestJsonWriter} and {@link SystemTestJsonReadser}.
 *
 */
public class TestSystemTestJson
  {
  @Test
  public void testSystemTest_0()
    {
    testSystemTestResource( "system-test-def-0.xml");
    }

  @Test
  public void testSystemTest_1()
    {
    testSystemTestResource( "system-test-def-1.xml");
    }

  @Test
  public void testSystemTest_2()
    {
    testSystemTestResource( "system-test-def-2.xml");
    }

  @Test
  public void testSystemTest_3()
    {
    testSystemTestResource( "system-test-def-3.xml");
    }

  @Test
  public void testSystemTest_4()
    {
    testSystemTestResource( "system-test-def-4.xml");
    }

  @Test
  public void testSystemTest_24()
    {
    testSystemTestResource( "system-test-def-24.xml");
    }

  @Test
  public void testSystemTest_find()
    {
    testSystemTestResource( "find-Test.xml");
    }

  @Test
  public void testSystemTest_Annotations_Missing()
    {
    assertValidationFailure(
      "system-test-annotations-missing.json",
      "The object must have at least 1 property(ies), but actual number is 0",
      functionDefCompatibility_);
    }

  @Test
  public void testSystemTest_Binding_NA_Invalid()
    {
    assertValidationFailure
      ( "system-test-binding-na-failure.json",
        "The object must not have a property whose name is \"NA\"",
        "The object must have a property whose name is \"value\"",
        "The object must not have a property whose name is \"failure\"",
        functionDefCompatibility_);
    
    assertValidationFailure
      ( "system-test-binding-na-value.json",
        "The object must not have a property whose name is \"NA\"",
        "The object must not have a property whose name is \"value\"",
        functionDefCompatibility_);
    }

  @Test
  public void testSystemTest_Binding_Property_Extra()
    {
    assertValidationFailure(
      "system-test-binding-property-extra.json",
      "The object must not have a property whose name is \"name\"",
      functionDefCompatibility_);
    }

  @Test
  public void testSystemTest_Binding_Value_Missing()
    {
    assertValidationFailure
      ( "system-test-binding-value-missing.json",
        "The object must have a property whose name is \"value\"",
        "The object must not have a property whose name is \"failure\"",
        "The object must have a property whose name is \"NA\"",
        functionDefCompatibility_);
    }

  @Test
  public void testSystemTest_Inputs_Missing()
    {
    assertValidationFailure(
      "system-test-inputs-missing.json",
      "The object must have at least 1 property(ies), but actual number is 0",
      functionDefCompatibility_);
    }

  @Test
  public void testSystemTest_System_Missing()
    {
    assertValidationFailure( "system-test-system-missing.json", "The object must have a property whose name is \"system\"");
    }

  @Test
  public void testSystemTest_TestCase_Id_Missing()
    {
    assertValidationFailure(
      "system-test-testcase-id-missing.json",
      "The object must have a property whose name is \"id\"",
      functionDefCompatibility_);
    }

  @Test
  public void testSystemTest_TestCase_Id_Invalid()
    {
    assertValidationFailure(
      "system-test-testcase-id-invalid.json",
      "The numeric value must be greater than or equal to 0",
      functionDefCompatibility_);
    }

  @Test
  public void testSystemTest_Failures_Multiple()
    {
    assertDefinitionError( "system-test-failures-multiple.json", "Can't have more than one variable bound to a value with failure=true");
    }

  @Test
  public void testSystemTest_Function_Name_Invalid()
    {
    assertDefinitionError( "system-test-function-name-invalid.json", "\"F(x)\" is not a valid identifier");
    }

  @Test
  public void testSystemTest_System_Invalid()
    {
    assertDefinitionError( "system-test-system-invalid.json", "\"System of Record\" is not a valid identifier");
    }

  @Test
  public void testSystemTest_Var_Name_Invalid()
    {
    assertDefinitionError( "system-test-var-name-invalid.json", "\"File Exists\" is not a valid identifier");
    }

  public void testSystemTestResource( String systemTestResource)
    {
    // Given...
    SystemTestDef systemTestBefore = systemTestResources_.read( systemTestResource);

    // When...
    ByteArrayOutputStream systemTestOut = new ByteArrayOutputStream();
    try( SystemTestJsonWriter writer = new SystemTestJsonWriter( systemTestOut))
      {
      writer.write( systemTestBefore);
      }

    SystemTestDef systemTestAfter;
    ByteArrayInputStream systemTestIn = new ByteArrayInputStream( systemTestOut.toByteArray());
    try( SystemTestJsonReader reader = new SystemTestJsonReader( systemTestIn))
      {
      systemTestAfter = reader.getSystemTestDef();
      }    

    // Then...
    assertThat( "Output from definition=" + systemTestResource, systemTestAfter, matches( new SystemTestDefMatcher( systemTestBefore)));
    assertThat( "Copy of definition=" + systemTestResource, SystemTestDefBuilder.with( systemTestAfter).build(), matches( new SystemTestDefMatcher( systemTestBefore)));
    }

  public void assertDefinitionError( String systemTestResource, String expected)
    {
    expectFailure( SystemTestException.class)
      .when( () -> systemTestResources_.readJson( systemTestResource))
      .then( failure -> {
        while( failure.getCause() != null)
          {
          assertThat( "Cause", failure.getCause().getClass(), equalTo( SystemTestException.class));
          failure = (SystemTestException) failure.getCause();
          }
        assertThat( "Reason", failure.getMessage(), containsString( expected));
        });
    }

  public void assertValidationFailure( String systemTestResource, String... expected)
    {
    expectFailure( SystemTestException.class)
      .when( () -> systemTestResources_.readJson( systemTestResource))
      .then( failure -> {
        Throwable cause = failure.getCause();
        assertThat( "Cause", cause.getClass(), equalTo( JsonValidatingException.class));

        JsonValidatingException jve = (JsonValidatingException)cause;
        List<String> problems = problems( jve).map( p -> p.getMessage()).collect( toList());
        assertThat( "Problems", problems, containsInAnyOrder( Arrays.stream( expected).map( m -> containsString(m)).collect( toList())));
        });
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

  private SystemTestResources systemTestResources_ = new SystemTestResources( TestSystemTestDocWriter.class);

  /**
   * A function test definition should be an object containing a "testCases" array. But for backward compatibility,
   * it can be specified by an array only. Consequently, when there is an error in the function object, schema
   * validation will suggest the array form as an alternative.
   */
  private static final String functionDefCompatibility_ = "The value must be of array type, but actual type is object";
  }
