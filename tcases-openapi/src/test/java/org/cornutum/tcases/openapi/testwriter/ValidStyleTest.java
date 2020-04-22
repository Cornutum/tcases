//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import org.cornutum.tcases.openapi.resolver.ParamData;
import static org.cornutum.tcases.openapi.resolver.DataValues.*;
import static org.cornutum.tcases.openapi.resolver.ParamDataBuilder.param;
import static org.cornutum.tcases.openapi.resolver.ParamDef.Location.*;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

/**
 * Runs tests for {@link TestWriterUtils#getValidStyle}.
 */
@SuppressWarnings("unchecked")
public class ValidStyleTest extends TestWriterTest
  {
  @Test
  public void whenQueryMatrixInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( QUERY).style( "matrix").integerData( 123).build();

    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenQueryLabelInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( QUERY).style( "label").integerData( 123).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenQueryFormInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( QUERY).style( "form").integerData( 123).build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenQueryFormNull()
    {
    // Given...
    ParamData param = param( "myParam").location( QUERY).style( "form").nullData().build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenQuerySimpleInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( QUERY).style( "simple").integerData( 123).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenQuerySpaceDelimitedInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( QUERY).style( "spaceDelimited").integerData( 123).build();

    // Then...
    expectInvalidStyleType( param);
    }
  
  @Test
  public void whenQuerySpaceDelimitedNull()
    {
    // Given...
    ParamData param = param( "myParam").location( QUERY).style( "spaceDelimited").nullData().build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenQueryPipeDelimitedInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( QUERY).style( "pipeDelimited").integerData( 123).build();

    // Then...
    expectInvalidStyleType( param);
    }
  
  @Test
  public void whenQueryPipeDelimitedNull()
    {
    // Given...
    ParamData param = param( "myParam").location( QUERY).style( "pipeDelimited").nullData().build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenQueryDeepObjectInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( QUERY).style( "deepObject").integerData( 123).build();

    // Then...
    expectInvalidStyleType( param);
    }
  
  @Test
  public void whenQueryDeepObjectNull()
    {
    // Given...
    ParamData param = param( "myParam").location( QUERY).style( "deepObject").nullData().build();

    // Then...
    expectValidStyle( param);
    }

  @Test
  public void whenQueryMatrixArray()
    {
    // Given...
    ParamData param = param( "myParam").location( QUERY).style( "matrix").arrayData( stringOf( "A"), stringOf( "B")).build();

    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenQueryLabelArray()
    {
    // Given...
    ParamData param = param( "myParam").location( QUERY).style( "label").arrayData( stringOf( "A"), stringOf( "B")).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenQueryFormArray()
    {
    // Given...
    ParamData param = param( "myParam").location( QUERY).style( "form").arrayData( stringOf( "A"), stringOf( "B")).build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenQuerySimpleArray()
    {
    // Given...
    ParamData param = param( "myParam").location( QUERY).style( "simple").arrayData( stringOf( "A"), stringOf( "B")).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenQuerySpaceDelimitedArray()
    {
    // Given...
    ParamData param = param( "myParam").location( QUERY).style( "spaceDelimited").arrayData( stringOf( "A"), stringOf( "B")).build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenQueryPipeDelimitedArray()
    {
    // Given...
    ParamData param = param( "myParam").location( QUERY).style( "pipeDelimited").arrayData( stringOf( "A"), stringOf( "B")).build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenQueryDeepObjectArray()
    {
    // Given...
    ParamData param = param( "myParam").location( QUERY).style( "deepObject").arrayData( stringOf( "A"), stringOf( "B")).build();

    // Then...
    expectInvalidStyleType( param);
    }
  
  @Test
  public void whenQueryMatrixObject()
    {
    // Given...
    ParamData param = param( "myParam").location( QUERY).style( "matrix").objectData( object().with( "name", stringOf( "X"))).build();

    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenQueryLabelObject()
    {
    // Given...
    ParamData param = param( "myParam").location( QUERY).style( "label").objectData( object().with( "name", stringOf( "X"))).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenQueryFormObject()
    {
    // Given...
    ParamData param = param( "myParam").location( QUERY).style( "form").objectData( object().with( "name", stringOf( "X"))).build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenQuerySimpleObject()
    {
    // Given...
    ParamData param = param( "myParam").location( QUERY).style( "simple").objectData( object().with( "name", stringOf( "X"))).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenQuerySpaceDelimitedObject()
    {
    // Given...
    ParamData param = param( "myParam").location( QUERY).style( "spaceDelimited").objectData( object().with( "name", stringOf( "X"))).build();

    // Then...
    expectInvalidStyleType( param);
    }
  
  @Test
  public void whenQueryPipeDelimitedObject()
    {
    // Given...
    ParamData param = param( "myParam").location( QUERY).style( "pipeDelimited").objectData( object().with( "name", stringOf( "X"))).build();

    // Then...
    expectInvalidStyleType( param);
    }
  
  @Test
  public void whenQueryDeepObjectObject()
    {
    // Given...
    ParamData param = param( "myParam").location( QUERY).style( "deepObject").objectData( object().with( "name", stringOf( "X"))).build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenPathMatrixInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( PATH).style( "matrix").integerData( 123).build();

    expectValidStyle( param);
    }
  
  @Test
  public void whenPathMatrixNull()
    {
    // Given...
    ParamData param = param( "myParam").location( PATH).style( "matrix").nullData().build();

    expectValidStyle( param);
    }
  
  @Test
  public void whenPathLabelInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( PATH).style( "label").integerData( 123).build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenPathLabelNull()
    {
    // Given...
    ParamData param = param( "myParam").location( PATH).style( "label").nullData().build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenPathFormInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( PATH).style( "form").integerData( 123).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenPathSimpleInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( PATH).style( "simple").integerData( 123).build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenPathSpaceDelimitedInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( PATH).style( "spaceDelimited").integerData( 123).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenPathPipeDelimitedInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( PATH).style( "pipeDelimited").integerData( 123).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenPathDeepObjectInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( PATH).style( "deepObject").integerData( 123).build();

    // Then...
    expectInvalidStyleLocation( param);
    }

  @Test
  public void whenPathMatrixArray()
    {
    // Given...
    ParamData param = param( "myParam").location( PATH).style( "matrix").arrayData( stringOf( "A"), stringOf( "B")).build();

    expectValidStyle( param);
    }
  
  @Test
  public void whenPathLabelArray()
    {
    // Given...
    ParamData param = param( "myParam").location( PATH).style( "label").arrayData( stringOf( "A"), stringOf( "B")).build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenPathFormArray()
    {
    // Given...
    ParamData param = param( "myParam").location( PATH).style( "form").arrayData( stringOf( "A"), stringOf( "B")).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenPathSimpleArray()
    {
    // Given...
    ParamData param = param( "myParam").location( PATH).style( "simple").arrayData( stringOf( "A"), stringOf( "B")).build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenPathSpaceDelimitedArray()
    {
    // Given...
    ParamData param = param( "myParam").location( PATH).style( "spaceDelimited").arrayData( stringOf( "A"), stringOf( "B")).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenPathPipeDelimitedArray()
    {
    // Given...
    ParamData param = param( "myParam").location( PATH).style( "pipeDelimited").arrayData( stringOf( "A"), stringOf( "B")).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenPathDeepObjectArray()
    {
    // Given...
    ParamData param = param( "myParam").location( PATH).style( "deepObject").arrayData( stringOf( "A"), stringOf( "B")).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenPathMatrixObject()
    {
    // Given...
    ParamData param = param( "myParam").location( PATH).style( "matrix").objectData( object().with( "name", stringOf( "X"))).build();

    expectValidStyle( param);
    }
  
  @Test
  public void whenPathLabelObject()
    {
    // Given...
    ParamData param = param( "myParam").location( PATH).style( "label").objectData( object().with( "name", stringOf( "X"))).build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenPathFormObject()
    {
    // Given...
    ParamData param = param( "myParam").location( PATH).style( "form").objectData( object().with( "name", stringOf( "X"))).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenPathSimpleObject()
    {
    // Given...
    ParamData param = param( "myParam").location( PATH).style( "simple").objectData( object().with( "name", stringOf( "X"))).build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenPathSpaceDelimitedObject()
    {
    // Given...
    ParamData param = param( "myParam").location( PATH).style( "spaceDelimited").objectData( object().with( "name", stringOf( "X"))).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenPathPipeDelimitedObject()
    {
    // Given...
    ParamData param = param( "myParam").location( PATH).style( "pipeDelimited").objectData( object().with( "name", stringOf( "X"))).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenPathDeepObjectObject()
    {
    // Given...
    ParamData param = param( "myParam").location( PATH).style( "deepObject").objectData( object().with( "name", stringOf( "X"))).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenHeaderMatrixInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( HEADER).style( "matrix").integerData( 123).build();

    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenHeaderLabelInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( HEADER).style( "label").integerData( 123).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenHeaderFormInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( HEADER).style( "form").integerData( 123).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenHeaderSimpleInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( HEADER).style( "simple").integerData( 123).build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenHeaderSimpleNull()
    {
    // Given...
    ParamData param = param( "myParam").location( HEADER).style( "simple").nullData().build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenHeaderSpaceDelimitedInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( HEADER).style( "spaceDelimited").integerData( 123).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenHeaderPipeDelimitedInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( HEADER).style( "pipeDelimited").integerData( 123).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenHeaderDeepObjectInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( HEADER).style( "deepObject").integerData( 123).build();

    // Then...
    expectInvalidStyleLocation( param);
    }

  @Test
  public void whenHeaderMatrixArray()
    {
    // Given...
    ParamData param = param( "myParam").location( HEADER).style( "matrix").arrayData( stringOf( "A"), stringOf( "B")).build();

    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenHeaderLabelArray()
    {
    // Given...
    ParamData param = param( "myParam").location( HEADER).style( "label").arrayData( stringOf( "A"), stringOf( "B")).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenHeaderFormArray()
    {
    // Given...
    ParamData param = param( "myParam").location( HEADER).style( "form").arrayData( stringOf( "A"), stringOf( "B")).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenHeaderSimpleArray()
    {
    // Given...
    ParamData param = param( "myParam").location( HEADER).style( "simple").arrayData( stringOf( "A"), stringOf( "B")).build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenHeaderSpaceDelimitedArray()
    {
    // Given...
    ParamData param = param( "myParam").location( HEADER).style( "spaceDelimited").arrayData( stringOf( "A"), stringOf( "B")).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenHeaderPipeDelimitedArray()
    {
    // Given...
    ParamData param = param( "myParam").location( HEADER).style( "pipeDelimited").arrayData( stringOf( "A"), stringOf( "B")).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenHeaderDeepObjectArray()
    {
    // Given...
    ParamData param = param( "myParam").location( HEADER).style( "deepObject").arrayData( stringOf( "A"), stringOf( "B")).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  @Test
  public void whenHeaderMatrixObject()
    {
    // Given...
    ParamData param = param( "myParam").location( HEADER).style( "matrix").objectData( object().with( "name", stringOf( "X"))).build();

    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenHeaderLabelObject()
    {
    // Given...
    ParamData param = param( "myParam").location( HEADER).style( "label").objectData( object().with( "name", stringOf( "X"))).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenHeaderFormObject()
    {
    // Given...
    ParamData param = param( "myParam").location( HEADER).style( "form").objectData( object().with( "name", stringOf( "X"))).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenHeaderSimpleObject()
    {
    // Given...
    ParamData param = param( "myParam").location( HEADER).style( "simple").objectData( object().with( "name", stringOf( "X"))).build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenHeaderSpaceDelimitedObject()
    {
    // Given...
    ParamData param = param( "myParam").location( HEADER).style( "spaceDelimited").objectData( object().with( "name", stringOf( "X"))).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenHeaderPipeDelimitedObject()
    {
    // Given...
    ParamData param = param( "myParam").location( HEADER).style( "pipeDelimited").objectData( object().with( "name", stringOf( "X"))).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenHeaderDeepObjectObject()
    {
    // Given...
    ParamData param = param( "myParam").location( HEADER).style( "deepObject").objectData( object().with( "name", stringOf( "X"))).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenCookieMatrixInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( COOKIE).style( "matrix").integerData( 123).build();

    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenCookieLabelInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( COOKIE).style( "label").integerData( 123).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenCookieFormInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( COOKIE).style( "form").integerData( 123).build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenCookieFormNull()
    {
    // Given...
    ParamData param = param( "myParam").location( COOKIE).style( "form").nullData().build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenCookieSimpleInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( COOKIE).style( "simple").integerData( 123).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenCookieSpaceDelimitedInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( COOKIE).style( "spaceDelimited").integerData( 123).build();

    // Then...
    expectInvalidStyleType( param);
    }
  
  @Test
  public void whenCookieSpaceDelimitedNull()
    {
    // Given...
    ParamData param = param( "myParam").location( COOKIE).style( "spaceDelimited").nullData().build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenCookiePipeDelimitedInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( COOKIE).style( "pipeDelimited").integerData( 123).build();

    // Then...
    expectInvalidStyleType( param);
    }
  
  @Test
  public void whenCookiePipeDelimitedNull()
    {
    // Given...
    ParamData param = param( "myParam").location( COOKIE).style( "pipeDelimited").nullData().build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenCookieDeepObjectInteger()
    {
    // Given...
    ParamData param = param( "myParam").location( COOKIE).style( "deepObject").integerData( 123).build();

    // Then...
    expectInvalidStyleType( param);
    }
  
  @Test
  public void whenCookieDeepObjectNull()
    {
    // Given...
    ParamData param = param( "myParam").location( COOKIE).style( "deepObject").nullData().build();

    // Then...
    expectValidStyle( param);
    }

  @Test
  public void whenCookieMatrixArray()
    {
    // Given...
    ParamData param = param( "myParam").location( COOKIE).style( "matrix").arrayData( stringOf( "A"), stringOf( "B")).build();

    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenCookieLabelArray()
    {
    // Given...
    ParamData param = param( "myParam").location( COOKIE).style( "label").arrayData( stringOf( "A"), stringOf( "B")).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenCookieFormArray()
    {
    // Given...
    ParamData param = param( "myParam").location( COOKIE).style( "form").arrayData( stringOf( "A"), stringOf( "B")).build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenCookieSimpleArray()
    {
    // Given...
    ParamData param = param( "myParam").location( COOKIE).style( "simple").arrayData( stringOf( "A"), stringOf( "B")).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenCookieSpaceDelimitedArray()
    {
    // Given...
    ParamData param = param( "myParam").location( COOKIE).style( "spaceDelimited").arrayData( stringOf( "A"), stringOf( "B")).build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenCookiePipeDelimitedArray()
    {
    // Given...
    ParamData param = param( "myParam").location( COOKIE).style( "pipeDelimited").arrayData( stringOf( "A"), stringOf( "B")).build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenCookieDeepObjectArray()
    {
    // Given...
    ParamData param = param( "myParam").location( COOKIE).style( "deepObject").arrayData( stringOf( "A"), stringOf( "B")).build();

    // Then...
    expectInvalidStyleType( param);
    }
  @Test
  public void whenCookieMatrixObject()
    {
    // Given...
    ParamData param = param( "myParam").location( COOKIE).style( "matrix").objectData( object().with( "name", stringOf( "X"))).build();

    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenCookieLabelObject()
    {
    // Given...
    ParamData param = param( "myParam").location( COOKIE).style( "label").objectData( object().with( "name", stringOf( "X"))).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenCookieFormObject()
    {
    // Given...
    ParamData param = param( "myParam").location( COOKIE).style( "form").objectData( object().with( "name", stringOf( "X"))).build();

    // Then...
    expectValidStyle( param);
    }
  
  @Test
  public void whenCookieSimpleObject()
    {
    // Given...
    ParamData param = param( "myParam").location( COOKIE).style( "simple").objectData( object().with( "name", stringOf( "X"))).build();

    // Then...
    expectInvalidStyleLocation( param);
    }
  
  @Test
  public void whenCookieSpaceDelimitedObject()
    {
    // Given...
    ParamData param = param( "myParam").location( COOKIE).style( "spaceDelimited").objectData( object().with( "name", stringOf( "X"))).build();

    // Then...
    expectInvalidStyleType( param);
    }
  
  @Test
  public void whenCookiePipeDelimitedObject()
    {
    // Given...
    ParamData param = param( "myParam").location( COOKIE).style( "pipeDelimited").objectData( object().with( "name", stringOf( "X"))).build();

    // Then...
    expectInvalidStyleType( param);
    }
  
  @Test
  public void whenCookieDeepObjectObject()
    {
    // Given...
    ParamData param = param( "myParam").location( COOKIE).style( "deepObject").objectData( object().with( "name", stringOf( "X"))).build();

    // Then...
    expectValidStyle( param);
    }

  private void expectValidStyle( ParamData param)
    {
    // When...
    String style = TestWriterUtils.getValidStyle( param);

    // Then...
    assertThat( String.format( "Valid %s style", param.getLocation()), style, is( param.getStyle()));
    }
  
  private void expectInvalidStyleType( ParamData param)
    {
    assertTestWriterException(
      () -> TestWriterUtils.getValidStyle( param),
      String.format( "Style=%s is not applicable for data type=%s", param.getStyle(), param.getType()));
    }

  private void expectInvalidStyleLocation( ParamData param)
    {
    assertTestWriterException(
      () -> TestWriterUtils.getValidStyle( param),
      String.format( "Style=%s is not applicable for a %s parameter", param.getStyle(), param.getLocation()));
    }
  }
