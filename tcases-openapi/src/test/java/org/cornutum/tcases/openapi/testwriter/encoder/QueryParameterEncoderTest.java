//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter.encoder;

import org.cornutum.tcases.openapi.resolver.ParamData;
import org.cornutum.tcases.openapi.testwriter.TestWriterUtils;
import org.cornutum.tcases.openapi.testwriter.encoder.UriEncoder.Component;
import org.cornutum.tcases.util.ListBuilder;
import static org.cornutum.tcases.openapi.resolver.DataValues.*;
import static org.cornutum.tcases.openapi.resolver.ParamDataBuilder.param;
import static org.cornutum.tcases.openapi.resolver.ParamDef.Location.*;
import static org.cornutum.tcases.openapi.testwriter.encoder.UriEncoder.uriEncoded;

import org.junit.Test;
import static org.cornutum.hamcrest.Composites.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.List;
import java.util.Map;
import java.util.AbstractMap.SimpleEntry;
import static java.util.Collections.emptyList;

/**
 * Runs tests for {@link FormParameterEncoder}.
 */
@SuppressWarnings("unchecked")
public class QueryParameterEncoderTest
  {  
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
      param( "my Param")
      .location( QUERY)
      .style( "form")
      .integerData( 123)
      .exploded()
      .build();

    // When...
    params = TestWriterUtils.getQueryParameters( param);
    
    // Then...
    assertThat(
      "Parameters",
      params,
      containsMembers(
        params()
        .binding( "my Param", "123")
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
    List<Map.Entry<String,String>> params = TestWriterUtils.getQueryParameters( param);
    
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
    params = TestWriterUtils.getQueryParameters( param);
    
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
  public void whenQueryFormArrayEmpty()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( QUERY)
      .style( "form")
      .arrayData()
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
      .arrayData()
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
      .objectData( object().with( "nick name", stringOf( "X")).with( "sex", stringOf( "?")))
      .build();

    // When...
    List<Map.Entry<String,String>> params = TestWriterUtils.getQueryParameters( param, true);
    
    // Then...
    assertThat(
      "Parameters",
      params,
      containsMembers(
        params()
        .encoding( "myParam", "nick name,X,sex,?")
        .build()));

    // Given...
    param =
      param( "myParam")
      .location( QUERY)
      .style( "form")
      .objectData( object().with( "nick name", stringOf( "X")).with( "sex", stringOf( "?")))
      .exploded()
      .build();

    // When...
    params = TestWriterUtils.getQueryParameters( param);
    
    // Then...
    assertThat(
      "Parameters",
      params,
      containsMembers(
        params()
        .binding( "nick name", "X")
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
      .objectData( object().with( "nick name", stringOf( "X")).with( "sex", stringOf( "?")))
      .build();

    // When...
    List<Map.Entry<String,String>> params = TestWriterUtils.getQueryParameters( param, true);
    
    // Then...
    assertThat(
      "Parameters",
      params,
      containsMembers(
        params()
        .encodingDeep( "myParam", "nick name", "X")
        .encodingDeep( "myParam", "sex", "?")
        .build()));

    // Given...
    param =
      param( "myParam")
      .location( QUERY)
      .style( "deepObject")
      .objectData( object().with( "nick name", stringOf( "X")).with( "sex", stringOf( "?")))
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
        .binding( "myParam[nick name]", "X")
        .binding( "myParam[sex]", "?")
        .build()));
    }

  private BindingsBuilder params()
    {
    return new BindingsBuilder( Component.QUERY);
    }
  
  private static class BindingsBuilder extends ListBuilder<Map.Entry<String,String>>
    {
    public BindingsBuilder( Component component)
      {
      component_ = component;
      }
    
    public BindingsBuilder binding( String name, String value)
      {
      add( new SimpleEntry<String,String>(name, value));
      return this;
      }
    
    public BindingsBuilder encoding( String name, String value)
      {
      return binding( uriEncoded( component_, name), uriEncoded( component_, value));
      }
    
    public BindingsBuilder encodingDeep( String name, String property, String value)
      {
      return binding( String.format( "%s[%s]", uriEncoded( component_, name), uriEncoded( component_, property)), uriEncoded( component_, value));
      }

    private Component component_;
    }

  }
