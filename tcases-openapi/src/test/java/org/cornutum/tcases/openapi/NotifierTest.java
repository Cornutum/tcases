//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.cornutum.tcases.SystemInputDef;
import org.cornutum.tcases.SystemTestDef;
import org.cornutum.tcases.Tcases;
import org.apache.commons.collections4.ListValuedMap;
import org.apache.commons.collections4.MultiMapUtils;
import org.junit.Test;
import static org.cornutum.hamcrest.Composites.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.Arrays;
import java.util.List;
import static java.util.Collections.emptyList;

/**
 * Runs tests for notifications using {@link ModelConditionNotifier}
 */
public class NotifierTest extends OpenApiTest
  {
  @Test
  public void warningWhenNoOpInputs()
    {
    // When...
    getTests( getRequestInputModel( "warnings-0"));
    
    // Then...
    assertWarnings( "Operations: No inputs to model for operation=OPTIONS_operations.");
    }
  
  @Test
  public void warningWhenNoMediaTypeSchema()
    {
    // When...
    getTests( getRequestInputModel( "warnings-1"));
    
    // Then...
    assertWarnings( "Operations,/operations,PATCH,requestBody,text/plain: No schema defined for media type=text/plain.");
    }
  
  @Test
  public void errorWhenUnknownSchemaType()
    {
    // When...
    getTests( getRequestInputModel( "errors-0"));
    
    // Then...
    assertErrors( "String,/string,POST,param0: Unknown schema type=decimal is not supported. Ignoring unknown schema.");
    }
  
  @Test
  public void errorWhenInfeasibleBounds()
    {
    // When...
    getTests( getRequestInputModel( "errors-2"));
    
    // Then...
    assertConditions(
      Arrays.asList(
        "Object,/object,POST,param0,myObject: minProperties=0 is superfluous -- same as required.",
        "Object,/object,POST,param0,myObject: maxProperties=0 is superfluous -- same as the total number of properties."),
      Arrays.asList(
        "Object,/object,POST,param0,myString: minLength=1 is greater than maxLength=0. Adjusting minLength to maxLength.",
        "Object,/object,POST,param0,myArray: minItems=10 is greater than maxItems=0. Adjusting minItems to maxItems.",
        "Object,/object,POST,param0,myObject: minProperties=1 is greater than maxProperties=0. Adjusting minProperties to maxProperties."));
    }
  
  @Test
  public void errorWhenUnusableBounds()
    {
    // When...
    getTests( getResponseInputModel( "errors-3"));
    
    // Then...
    assertConditions(
      Arrays.asList(
        "Responses,/responses,GET,2XX,content,application/json,alpha: maxProperties=2 is superfluous -- same as required.",
        "Responses,/responses,GET,2XX,content,application/json,charlie: minProperties=2 is superfluous -- same as required.",
        "Responses,/responses,GET,2XX,content,application/json,charlie: maxProperties=3 is superfluous -- same as the total number of properties."),
      Arrays.asList(
        "Responses,/responses,GET,2XX,content,application/json,alpha: minProperties=1 is less than required=2. Ignoring infeasible minProperties.",
        "Responses,/responses,GET,2XX,content,application/json,bravo: maxProperties=4 exceeds the total number of properties. Ignoring infeasible maxProperties.",
        "Responses,/responses,GET,2XX,content,application/json,delta: minProperties=4 exceeds the total number of properties. Ignoring infeasible minProperties.",
        "Responses,/responses,GET,2XX,content,application/json,delta: maxProperties=5 exceeds the total number of properties. Ignoring infeasible maxProperties."));
    }
  
  @Test
  public void errorWhenInfeasibleRange()
    {
    // When...
    getTests( getRequestInputModel( "errors-4"));
    
    // Then...
    assertErrors(
      "Numbers,/numbers,POST,param0: minimum=1000 is greater than maximum=99. Adjusting minimum to maximum.",
      "Numbers,/numbers,POST,param1: minimum=1231.23 is greater than maximum=121.77. Adjusting minimum to maximum.");
    }

  private SystemInputDef getRequestInputModel( String resource)
    {
    // When...
    SystemInputDef inputDef = getRequestInputModel( readApi( resource));

    // Then...
    assertThat( "Input model", inputDef, is( notNullValue()));

    return inputDef;
    }

  private SystemInputDef getResponseInputModel( String resource)
    {
    // When...
    SystemInputDef inputDef = getResponseInputModel( readApi( resource));

    // Then...
    assertThat( "Input model", inputDef, is( notNullValue()));

    return inputDef;
    }

  private SystemTestDef getTests( SystemInputDef inputDef)
    {
    // When...
    SystemTestDef testDef = Tcases.getTests( inputDef, null, null);

    // Then...
    assertThat( "Test model", testDef, is( notNullValue()));

    return testDef;
    }

  /**
   * Returns the {@link ModelOptions} used for this test.
   */
  protected ModelOptions getModelOptions()
    {
    return ModelOptions.builder().notifier( conditionRecorder_).build();
    }
  
  private void assertWarnings( String... warnings)
    {
    assertThat( "Warnings", conditionRecorder_.getWarnings(), listsMembers( warnings));
    assertThat( "Errors", conditionRecorder_.getErrors(), listsMembers( emptyList()));
    }

  private void assertErrors( String... errors)
    {
    assertThat( "Errors", conditionRecorder_.getErrors(), listsMembers( errors));
    assertThat( "Warnings", conditionRecorder_.getWarnings(), listsMembers( emptyList()));
    }
  
  private void assertConditions( List<String> warnings, List<String> errors)
    {
    assertThat( "Warnings", conditionRecorder_.getWarnings(), listsMembers( warnings));
    assertThat( "Errors", conditionRecorder_.getErrors(), listsMembers( errors));
    }
  
  private static class ModelConditionRecorder implements ModelConditionNotifier
    {
    public void warn( String[] location, String reason)
      {
      conditions_.put( "warn", messageFor( location, reason, null));
      }

    public void error( String[] location, String reason, String resolution)
      {
      conditions_.put( "error", messageFor( location, reason, resolution));
      }

    public List<String> getWarnings()
      {
      return conditions_.get( "warn");
      }

    public List<String> getErrors()
      {
      return conditions_.get( "error");
      }
      
    private ListValuedMap<String,String> conditions_ = MultiMapUtils.newListValuedHashMap();
    }
  
  private ModelConditionRecorder conditionRecorder_ = new ModelConditionRecorder();
  }
