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
import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.Arrays;

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
        "Object,/object,POST,param0: style=form is not applicable for a header parameter. Using style=simple instead.",
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
  
  @Test
  public void errorWhenInfeasibleUniqueItems()
    {
    // When...
    getTests( getRequestInputModel( "errors-5"));
    
    // Then...
    assertErrors(
      "Arrays,/arrays,POST,param0: maxItems=10 can exceed the number of unique item values possible. Adjusting to unique maxItems=8.",
      "Arrays,/arrays,POST,param1: minItems=4 can exceed the number of unique item values possible. Adjusting to unique minItems=3.",
      "Arrays,/arrays,POST,param1: maxItems=null can exceed the number of unique item values possible. Adjusting to unique maxItems=3.");
    }
  
  @Test
  public void errorWhenInvalidStyles()
    {
    // When...
    getTests( getRequestInputModel( "styles"));
    
    // Then...
    assertErrors(
      "Styles,/query/integer,GET,matrixParam: style=matrix is not applicable for a query parameter. Using style=form instead.",
      "Styles,/query/integer,GET,labelParam: style=label is not applicable for a query parameter. Using style=form instead.",
      "Styles,/query/integer,GET,simpleParam: style=simple is not applicable for a query parameter. Using style=form instead.",
      "Styles,/query/integer,GET,spaceDelimitedParam: style=spaceDelimited is not applicable for parameter type=integer. Using style=form instead.",
      "Styles,/query/integer,GET,pipeDelimitedParam: style=pipeDelimited is not applicable for parameter type=integer. Using style=form instead.",
      "Styles,/query/integer,GET,deepObjectParam: style=deepObject is not applicable for parameter type=integer. Using style=form instead.",
      "Styles,/query/array,GET,matrixParam: style=matrix is not applicable for a query parameter. Using style=form instead.",
      "Styles,/query/array,GET,labelParam: style=label is not applicable for a query parameter. Using style=form instead.",
      "Styles,/query/array,GET,simpleParam: style=simple is not applicable for a query parameter. Using style=form instead.",
      "Styles,/query/array,GET,deepObjectParam: style=deepObject is not applicable for parameter type=array. Using style=form instead.",
      "Styles,/query/object,GET,matrixParam: style=matrix is not applicable for a query parameter. Using style=form instead.",
      "Styles,/query/object,GET,labelParam: style=label is not applicable for a query parameter. Using style=form instead.",
      "Styles,/query/object,GET,simpleParam: style=simple is not applicable for a query parameter. Using style=form instead.",
      "Styles,/query/object,GET,spaceDelimitedParam: style=spaceDelimited is not applicable for parameter type=object. Using style=form instead.",
      "Styles,/query/object,GET,pipeDelimitedParam: style=pipeDelimited is not applicable for parameter type=object. Using style=form instead.",
      "Styles,/header/integer,GET,matrixParam: style=matrix is not applicable for a header parameter. Using style=simple instead.",
      "Styles,/header/integer,GET,labelParam: style=label is not applicable for a header parameter. Using style=simple instead.",
      "Styles,/header/integer,GET,formParam: style=form is not applicable for a header parameter. Using style=simple instead.",
      "Styles,/header/integer,GET,spaceDelimitedParam: style=spaceDelimited is not applicable for a header parameter. Using style=simple instead.",
      "Styles,/header/integer,GET,pipeDelimitedParam: style=pipeDelimited is not applicable for a header parameter. Using style=simple instead.",
      "Styles,/header/integer,GET,deepObjectParam: style=deepObject is not applicable for a header parameter. Using style=simple instead.",
      "Styles,/header/array,GET,matrixParam: style=matrix is not applicable for a header parameter. Using style=simple instead.",
      "Styles,/header/array,GET,labelParam: style=label is not applicable for a header parameter. Using style=simple instead.",
      "Styles,/header/array,GET,formParam: style=form is not applicable for a header parameter. Using style=simple instead.",
      "Styles,/header/array,GET,spaceDelimitedParam: style=spaceDelimited is not applicable for a header parameter. Using style=simple instead.",
      "Styles,/header/array,GET,pipeDelimitedParam: style=pipeDelimited is not applicable for a header parameter. Using style=simple instead.",
      "Styles,/header/array,GET,deepObjectParam: style=deepObject is not applicable for a header parameter. Using style=simple instead.",
      "Styles,/header/object,GET,matrixParam: style=matrix is not applicable for a header parameter. Using style=simple instead.",
      "Styles,/header/object,GET,labelParam: style=label is not applicable for a header parameter. Using style=simple instead.",
      "Styles,/header/object,GET,formParam: style=form is not applicable for a header parameter. Using style=simple instead.",
      "Styles,/header/object,GET,spaceDelimitedParam: style=spaceDelimited is not applicable for a header parameter. Using style=simple instead.",
      "Styles,/header/object,GET,pipeDelimitedParam: style=pipeDelimited is not applicable for a header parameter. Using style=simple instead.",
      "Styles,/header/object,GET,deepObjectParam: style=deepObject is not applicable for a header parameter. Using style=simple instead.",
      "Styles,/cookie/integer,GET,matrixParam: style=matrix is not applicable for a cookie parameter. Using style=form instead.",
      "Styles,/cookie/integer,GET,labelParam: style=label is not applicable for a cookie parameter. Using style=form instead.",
      "Styles,/cookie/integer,GET,simpleParam: style=simple is not applicable for a cookie parameter. Using style=form instead.",
      "Styles,/cookie/integer,GET,spaceDelimitedParam: style=spaceDelimited is not applicable for parameter type=integer. Using style=form instead.",
      "Styles,/cookie/integer,GET,pipeDelimitedParam: style=pipeDelimited is not applicable for parameter type=integer. Using style=form instead.",
      "Styles,/cookie/integer,GET,deepObjectParam: style=deepObject is not applicable for parameter type=integer. Using style=form instead.",
      "Styles,/cookie/array,GET,matrixParam: style=matrix is not applicable for a cookie parameter. Using style=form instead.",
      "Styles,/cookie/array,GET,labelParam: style=label is not applicable for a cookie parameter. Using style=form instead.",
      "Styles,/cookie/array,GET,simpleParam: style=simple is not applicable for a cookie parameter. Using style=form instead.",
      "Styles,/cookie/array,GET,deepObjectParam: style=deepObject is not applicable for parameter type=array. Using style=form instead.",
      "Styles,/cookie/object,GET,matrixParam: style=matrix is not applicable for a cookie parameter. Using style=form instead.",
      "Styles,/cookie/object,GET,labelParam: style=label is not applicable for a cookie parameter. Using style=form instead.",
      "Styles,/cookie/object,GET,simpleParam: style=simple is not applicable for a cookie parameter. Using style=form instead.",
      "Styles,/cookie/object,GET,spaceDelimitedParam: style=spaceDelimited is not applicable for parameter type=object. Using style=form instead.",
      "Styles,/cookie/object,GET,pipeDelimitedParam: style=pipeDelimited is not applicable for parameter type=object. Using style=form instead.",
      "Styles,/path/integer/{matrixParam}/{labelParam}/{formParam}/{simpleParam}/{spaceDelimitedParam}/{pipeDelimitedParam}/{deepObjectParam},GET,formParam: style=form is not applicable for a path parameter. Using style=simple instead.",
      "Styles,/path/integer/{matrixParam}/{labelParam}/{formParam}/{simpleParam}/{spaceDelimitedParam}/{pipeDelimitedParam}/{deepObjectParam},GET,spaceDelimitedParam: style=spaceDelimited is not applicable for a path parameter. Using style=simple instead.",
      "Styles,/path/integer/{matrixParam}/{labelParam}/{formParam}/{simpleParam}/{spaceDelimitedParam}/{pipeDelimitedParam}/{deepObjectParam},GET,pipeDelimitedParam: style=pipeDelimited is not applicable for a path parameter. Using style=simple instead.",
      "Styles,/path/integer/{matrixParam}/{labelParam}/{formParam}/{simpleParam}/{spaceDelimitedParam}/{pipeDelimitedParam}/{deepObjectParam},GET,deepObjectParam: style=deepObject is not applicable for a path parameter. Using style=simple instead.",
      "Styles,/path/array/{matrixParam}/{labelParam}/{formParam}/{simpleParam}/{spaceDelimitedParam}/{pipeDelimitedParam}/{deepObjectParam},GET,formParam: style=form is not applicable for a path parameter. Using style=simple instead.",
      "Styles,/path/array/{matrixParam}/{labelParam}/{formParam}/{simpleParam}/{spaceDelimitedParam}/{pipeDelimitedParam}/{deepObjectParam},GET,spaceDelimitedParam: style=spaceDelimited is not applicable for a path parameter. Using style=simple instead.",
      "Styles,/path/array/{matrixParam}/{labelParam}/{formParam}/{simpleParam}/{spaceDelimitedParam}/{pipeDelimitedParam}/{deepObjectParam},GET,pipeDelimitedParam: style=pipeDelimited is not applicable for a path parameter. Using style=simple instead.",
      "Styles,/path/array/{matrixParam}/{labelParam}/{formParam}/{simpleParam}/{spaceDelimitedParam}/{pipeDelimitedParam}/{deepObjectParam},GET,deepObjectParam: style=deepObject is not applicable for a path parameter. Using style=simple instead.",
      "Styles,/path/object/{matrixParam}/{labelParam}/{formParam}/{simpleParam}/{spaceDelimitedParam}/{pipeDelimitedParam}/{deepObjectParam},GET,formParam: style=form is not applicable for a path parameter. Using style=simple instead.",
      "Styles,/path/object/{matrixParam}/{labelParam}/{formParam}/{simpleParam}/{spaceDelimitedParam}/{pipeDelimitedParam}/{deepObjectParam},GET,spaceDelimitedParam: style=spaceDelimited is not applicable for a path parameter. Using style=simple instead.",
      "Styles,/path/object/{matrixParam}/{labelParam}/{formParam}/{simpleParam}/{spaceDelimitedParam}/{pipeDelimitedParam}/{deepObjectParam},GET,pipeDelimitedParam: style=pipeDelimited is not applicable for a path parameter. Using style=simple instead.",
      "Styles,/path/object/{matrixParam}/{labelParam}/{formParam}/{simpleParam}/{spaceDelimitedParam}/{pipeDelimitedParam}/{deepObjectParam},GET,deepObjectParam: style=deepObject is not applicable for a path parameter. Using style=simple instead.");
    }
  
  @Test
  public void failureWhenInvalidParamName()
    {
    assertOpenApiException(
      () -> getTests( getRequestInputModel( "characters-0")),
      "Error processing Characters, /password, POST, <password>",
      "Parameter name='<password>' contains characters not allowed in a cookie name");
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
    return withConditionRecorder();
    }
  }
