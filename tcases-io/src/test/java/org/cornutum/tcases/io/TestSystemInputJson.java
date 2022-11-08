//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.*;
import static org.cornutum.hamcrest.Composites.*;
import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;

/**
 * Runs tests for the {@link SystemInputJsonWriter} and {@link SystemInputJsonReadser}.
 *
 */
public class TestSystemInputJson extends SystemInputJsonTest
  {
  @Test
  public void testSystemInput_0()
    {
    testSystemInputResource( "system-input-def-0.xml");
    }

  @Test
  public void testSystemInput_1()
    {
    testSystemInputResource( "system-input-def-1.xml");
    }

  @Test
  public void testSystemInput_2()
    {
    testSystemInputResource( "system-input-def-2.xml");
    }

  @Test
  public void testSystemInput_3()
    {
    testSystemInputResource( "system-input-def-3.xml");
    }

  @Test
  public void testSystemInput_4()
    {
    testSystemInputResource( "system-input-def-4.xml");
    }

  @Test
  public void testSystemInput_5()
    {
    testSystemInputResource( "system-input-def-5.xml");
    }

  @Test
  public void testSystemInput_6()
    {
    testSystemInputResource( "system-input-def-6.xml");
    }

  @Test
  public void testSystemInput_7()
    {
    testSystemInputResource( "system-input-def-7.xml");
    }

  @Test
  public void testSystemInput_43()
    {
    testSystemInputResource( "system-input-def-43.xml");
    }

  @Test
  public void testSystemInput_objects()
    {
    testSystemInputResource( "system-input-def-objects.xml");
    }

  @Test
  public void testSystemInput_BoundedAssertions()
    {
    testSystemInputResource( "Ice-Cream-Input.xml");
    testSystemInputResource( "system-input-def-min-max.xml");
    }

  @Test
  public void testSystemInput_hasNone()
    {
    // Given...
    String jsonInputResource = "find-Input.json";
    SystemInputDef systemInputBefore = systemInputResources_.readJson( jsonInputResource);

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
    assertThat( "Output from definition=" + jsonInputResource, systemInputAfter, matches( new SystemInputDefMatcher( systemInputBefore)));

    // Given...
    String xmlInputResource = "find-Input.xml";
    SystemInputDef systemInputXml = systemInputResources_.read( xmlInputResource);

    // Then...
    assertThat( "Comparing " + jsonInputResource + " and " + xmlInputResource, systemInputAfter, matches( new SystemInputDefMatcher( systemInputXml)));
    }

  @Test
  public void testSystemInput_Annotations_Missing()
    {
    assertValidationFailure( "system-input-annotations-missing.json", "The object must have at least 1 property(ies), but actual number is 0");
    }

  @Test
  public void testSystemInput_Condition_Extra()
    {
    assertValidationFailure( "system-input-condition-extra.json", "The object must have at most 1 property(ies), but actual number is 2");
    }

  @Test
  public void testSystemInput_Condition_Missing()
    {
    assertValidationFailure( "system-input-condition-missing.json", "The object must have at least 1 property(ies), but actual number is 0");
    }

  @Test
  public void testSystemInput_Conditions_Missing()
    {
    assertValidationFailure( "system-input-conditions-missing.json", "The array must have at least 1 element(s), but actual number is 0");
    }

  @Test
  public void testSystemInput_Members_Missing()
    {
    assertValidationFailure( "system-input-members-missing.json", "The object must have at least 1 property(ies), but actual number is 0");
    }

  @Test
  public void testSystemInput_Name_Missing()
    {
    assertValidationFailure( "system-input-name-missing.json", "The object must have a property whose name is \"system\"");
    }

  @Test
  public void testSystemInput_Properties_Missing()
    {
    assertValidationFailure( "system-input-properties-missing.json", "The array must have at least 1 element(s), but actual number is 0");
    }

  @Test
  public void testSystemInput_Property_Duplicated()
    {
    assertValidationFailure( "system-input-property-duplicated.json", "The array must consists only of unique elements, but the element at [1] is the same as the element at [0]");
    }

  @Test
  public void testSystemInput_Values_Missing()
    {
    assertValidationFailure( "system-input-values-missing.json", "The object must have at least 1 property(ies), but actual number is 0");
    }

  @Test
  public void testSystemInput_Var_Invalid()
    {
    assertValidationFailure
      ( "system-input-var-invalid.json",
        "The object must not have a property whose name is \"values\"",
        "The object must not have a property whose name is \"members\"");
    }

  @Test
  public void testSystemInput_Condition_Invalid()
    {
    assertDefinitionError( "system-input-condition-invalid.json", "\"\" is not a valid identifier");
    }

  @Test
  public void testSystemInput_Failure_Properties()
    {
    assertDefinitionError( "system-input-failure-properties.json", "Failure type values can't define properties");
    }

  @Test
  public void testSystemInput_Name_Invalid()
    {
    assertDefinitionError( "system-input-name-invalid.json", "\"My System\" is not a valid identifier");
    }

  @Test
  public void testSystemInput_Function_Name_Invalid()
    {
    assertDefinitionError( "system-input-function-name-invalid.json", "\"F(x)\" is not a valid identifier");
    }

  @Test
  public void testSystemInput_Var_Name_Invalid()
    {
    assertDefinitionError( "system-input-var-name-invalid.json", "\"a.b.c.d\" is not a valid identifier");
    }

  @Test
  public void testSystemInput_Property_Name_Invalid()
    {
    assertDefinitionError( "system-input-property-name-invalid.json", "\"a certain quality\" is not a valid identifier");
    }

  @Test
  public void testSystemInput_Property_Undefined()
    {
    assertDefinitionError( "system-input-property-undefined.json", "Property=small is undefined, but referenced by variable=Color");
    }

  @Test
  public void testSystemInput_Vars_Missing()
    {
    assertDefinitionError( "system-input-vars-missing.json", "No variables defined for function=Make");
    }
  }
