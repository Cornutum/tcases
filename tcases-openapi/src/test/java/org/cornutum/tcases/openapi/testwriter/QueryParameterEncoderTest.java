//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import org.cornutum.tcases.openapi.resolver.ParamData;
import org.cornutum.tcases.util.ListBuilder;

import static org.cornutum.tcases.openapi.resolver.DataValues.*;
import static org.cornutum.tcases.openapi.resolver.ParamDataBuilder.param;
import static org.cornutum.tcases.openapi.resolver.ParamDef.Location.*;

import org.junit.Test;
import static org.cornutum.hamcrest.Composites.*;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.List;
import java.util.Map;
import java.net.URLEncoder;
import java.util.AbstractMap.SimpleEntry;
import static java.util.Collections.emptyList;

/**
 * Runs tests for {@link TestWriterUtils.QueryParameterEncoder}.
 */
@SuppressWarnings("unchecked")
public class QueryParameterEncoderTest
  {
  @Test
  public void whenQueryMatrixInteger()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( QUERY)
      .style( "matrix")
      .integerData( 123)
      .build();

    expectFailure( TestWriterException.class)
      .when( () -> TestWriterUtils.getQueryParameters( param, false))
      .then( failure -> {
        assertThat(
          "Failure",
          failure.getMessage(),
          is( String.format( "Style=%s is not applicable for a %s parameter", param.getStyle(), param.getLocation())));
        });
    }
  
  @Test
  public void whenPathFormInteger()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( PATH)
      .style( "form")
      .integerData( 123)
      .build();

    expectFailure( TestWriterException.class)
      .when( () -> TestWriterUtils.getQueryParameters( param, false))
      .then( failure -> {
        assertThat(
          "Failure",
          failure.getMessage(),
          is( String.format( "%s is not a QUERY parameter", param)));
        });
    }
  
  @Test
  public void whenQueryFormInteger()
    {
    // Given...
    ParamData param =
      param( "my Param")
      .location( QUERY)
      .style( "form")
      .integerData( 123)
      .build();

    // When...
    List<Map.Entry<String,String>> params = TestWriterUtils.getQueryParameters( param, true);
    
    // Then...
    assertThat(
      "Parameters",
      params,
      containsMembers(
        params()
        .encoding( "my Param", "123")
        .build()));

    // Given...
    param =
      param( "myParam")
      .location( QUERY)
      .style( "form")
      .integerData( 123)
      .exploded()
      .build();

    // When...
    params = TestWriterUtils.getQueryParameters( param, false);
    
    // Then...
    assertThat(
      "Parameters",
      params,
      containsMembers(
        params()
        .binding( "myParam", "123")
        .build()));
    }
  
  @Test
  public void whenQueryFormUndefined()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( QUERY)
      .style( "form")
      .build()
      ;

    // When...
    List<Map.Entry<String,String>> params = TestWriterUtils.getQueryParameters( param, true);
    
    // Then...
    assertThat( "Parameters", params, is( emptyList()));
    }
  
  @Test
  public void whenQueryFormNull()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( QUERY)
      .style( "form")
      .nullData()
      .build();

    // When...
    List<Map.Entry<String,String>> params = TestWriterUtils.getQueryParameters( param, true);
    
    // Then...
    assertThat(
      "Parameters",
      params,
      containsMembers(
        params()
        .encoding( "myParam", "")
        .build()));

    // Given...
    param =
      param( "myParam")
      .location( QUERY)
      .style( "form")
      .nullData()
      .exploded()
      .build();

    // When...
    params = TestWriterUtils.getQueryParameters( param, false);
    
    // Then...
    assertThat(
      "Parameters",
      params,
      containsMembers(
        params()
        .binding( "myParam", "")
        .build()));
    }
  
  @Test
  public void whenQuerySpaceDelimitedNull()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( QUERY)
      .style( "spaceDelimited")
      .nullData()
      .build();

    // When...
    List<Map.Entry<String,String>> params = TestWriterUtils.getQueryParameters( param, true);
    
    // Then...
    assertThat(
      "Parameters",
      params,
      containsMembers(
        params()
        .encoding( "myParam", "")
        .build()));

    // Given...
    param =
      param( "myParam")
      .location( QUERY)
      .style( "spaceDelimited")
      .nullData()
      .exploded()
      .build();

    // When...
    params = TestWriterUtils.getQueryParameters( param, false);
    
    // Then...
    assertThat(
      "Parameters",
      params,
      containsMembers(
        params()
        .binding( "myParam", "")
        .build()));
    }
  
  @Test
  public void whenQueryPipeDelimitedNull()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( QUERY)
      .style( "pipeDelimited")
      .nullData()
      .build();

    // When...
    List<Map.Entry<String,String>> params = TestWriterUtils.getQueryParameters( param, true);
    
    // Then...
    assertThat(
      "Parameters",
      params,
      containsMembers(
        params()
        .encoding( "myParam", "")
        .build()));

    // Given...
    param =
      param( "myParam")
      .location( QUERY)
      .style( "pipeDelimited")
      .nullData()
      .exploded()
      .build();

    // When...
    params = TestWriterUtils.getQueryParameters( param, false);
    
    // Then...
    assertThat(
      "Parameters",
      params,
      containsMembers(
        params()
        .binding( "myParam", "")
        .build()));
    }
  
  @Test
  public void whenQueryDeepObjectNull()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( QUERY)
      .style( "deepObject")
      .nullData()
      .build();

    // When...
    List<Map.Entry<String,String>> params = TestWriterUtils.getQueryParameters( param, true);
    
    // Then...
    assertThat(
      "Parameters",
      params,
      containsMembers(
        params()
        .encoding( "myParam", "")
        .build()));

    // Given...
    param =
      param( "myParam")
      .location( QUERY)
      .style( "deepObject")
      .nullData()
      .exploded()
      .build();

    // When...
    params = TestWriterUtils.getQueryParameters( param, false);
    
    // Then...
    assertThat(
      "Parameters",
      params,
      containsMembers(
        params()
        .binding( "myParam", "")
        .build()));
    }
  
  @Test
  public void whenQueryFormArray()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( QUERY)
      .style( "form")
      .arrayData( stringOf( "A"), stringOf( "B"))
      .build();

    // When...
    List<Map.Entry<String,String>> params = TestWriterUtils.getQueryParameters( param, true);
    
    // Then...
    assertThat(
      "Parameters",
      params,
      containsMembers(
        params()
        .encoding( "myParam", "A,B")
        .build()));

    // Given...
    param =
      param( "myParam")
      .location( QUERY)
      .style( "form")
      .arrayData( stringOf( "A"), stringOf( "B"))
      .exploded()
      .build();

    // When...
    params = TestWriterUtils.getQueryParameters( param, false);
    
    // Then...
    assertThat(
      "Parameters",
      params,
      containsMembers(
        params()
        .binding( "myParam", "A")
        .binding( "myParam", "B")
        .build()));
    }
  
  @Test
  public void whenQuerySpaceDelimitedArray()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( QUERY)
      .style( "spaceDelimited")
      .arrayData( stringOf( "A"), stringOf( "B"))
      .build();

    // When...
    List<Map.Entry<String,String>> params = TestWriterUtils.getQueryParameters( param, true);
    
    // Then...
    assertThat(
      "Parameters",
      params,
      containsMembers(
        params()
        .encoding( "myParam", "A B")
        .build()));

    // Given...
    param =
      param( "myParam")
      .location( QUERY)
      .style( "spaceDelimited")
      .arrayData( stringOf( "A"), stringOf( "B"))
      .exploded()
      .build();

    // When...
    params = TestWriterUtils.getQueryParameters( param, false);
    
    // Then...
    assertThat(
      "Parameters",
      params,
      containsMembers(
        params()
        .binding( "myParam", "A")
        .binding( "myParam", "B")
        .build()));
    }
  
  @Test
  public void whenQueryPipeDelimitedArray()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( QUERY)
      .style( "pipeDelimited")
      .arrayData( stringOf( "A"), stringOf( "B"))
      .build();

    // When...
    List<Map.Entry<String,String>> params = TestWriterUtils.getQueryParameters( param, true);
    
    // Then...
    assertThat(
      "Parameters",
      params,
      containsMembers(
        params()
        .encoding( "myParam", "A|B")
        .build()));

    // Given...
    param =
      param( "myParam")
      .location( QUERY)
      .style( "pipeDelimited")
      .arrayData( stringOf( "A"), stringOf( "B"))
      .exploded()
      .build();

    // When...
    params = TestWriterUtils.getQueryParameters( param, false);
    
    // Then...
    assertThat(
      "Parameters",
      params,
      containsMembers(
        params()
        .binding( "myParam", "A")
        .binding( "myParam", "B")
        .build()));
    }
  
  @Test
  public void whenQueryFormObject()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( QUERY)
      .style( "form")
      .objectData( object().with( "name", stringOf( "X")).with( "sex", stringOf( "?")))
      .build();

    // When...
    List<Map.Entry<String,String>> params = TestWriterUtils.getQueryParameters( param, true);
    
    // Then...
    assertThat(
      "Parameters",
      params,
      containsMembers(
        params()
        .encoding( "myParam", "name,X,sex,?")
        .build()));

    // Given...
    param =
      param( "myParam")
      .location( QUERY)
      .style( "form")
      .objectData( object().with( "name", stringOf( "X")).with( "sex", stringOf( "?")))
      .exploded()
      .build();

    // When...
    params = TestWriterUtils.getQueryParameters( param, false);
    
    // Then...
    assertThat(
      "Parameters",
      params,
      containsMembers(
        params()
        .binding( "name", "X")
        .binding( "sex", "?")
        .build()));
    }
  
  @Test
  public void whenQueryDeepObjectObject()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( QUERY)
      .style( "deepObject")
      .objectData( object().with( "name", stringOf( "X")).with( "sex", stringOf( "?")))
      .build();

    // When...
    List<Map.Entry<String,String>> params = TestWriterUtils.getQueryParameters( param, true);
    
    // Then...
    assertThat(
      "Parameters",
      params,
      containsMembers(
        params()
        .encoding( "myParam[name]", "X")
        .encoding( "myParam[sex]", "?")
        .build()));

    // Given...
    param =
      param( "myParam")
      .location( QUERY)
      .style( "deepObject")
      .objectData( object().with( "name", stringOf( "X")).with( "sex", stringOf( "?")))
      .exploded()
      .build();

    // When...
    params = TestWriterUtils.getQueryParameters( param, false);
    
    // Then...
    assertThat(
      "Parameters",
      params,
      containsMembers(
        params()
        .binding( "myParam[name]", "X")
        .binding( "myParam[sex]", "?")
        .build()));
    }

  private BindingsBuilder params()
    {
    return new BindingsBuilder();
    }
  
  private static class BindingsBuilder extends ListBuilder<Map.Entry<String,String>>
    {
    public BindingsBuilder binding( String name, String value)
      {
      add( new SimpleEntry<String,String>(name, value));
      return this;
      }
    
    public BindingsBuilder encoding( String name, String value)
      {
      return binding( encode( name), encode( value));
      }

    private String encode( String value)
      {
      try
        {
        return URLEncoder.encode( value, "UTF-8");
        }
      catch( Exception e)
        {
        throw new IllegalArgumentException( String.format( "Can't encode value='%s'", value), e);
        }
      }
    }

  }
