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

import java.math.BigDecimal;

/**
 * Runs tests for {@link TestWriterUtils.SimpleValueEncoder}.
 */
@SuppressWarnings("unchecked")
public class SimpleValueEncoderTest extends TestWriterTest
  {
  @Test
  public void whenPathStyleInvalid()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( PATH)
      .style( "form")
      .decimalData( new BigDecimal( "123.45"))
      .build();

    assertTestWriterException(
      () -> TestWriterUtils.getPathParameterValue( param),
      String.format( "%s: can't get path parameter value", param),
      String.format( "style=%s is not applicable for a %s parameter", param.getStyle(), String.valueOf( param.getLocation()).toLowerCase()));
    }
  
  @Test
  public void whenSimpleDecimal()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( PATH)
      .style( "simple")
      .decimalData( new BigDecimal( "123.45"))
      .build();

    // When...
    String encoded = TestWriterUtils.getPathParameterValue( param);
    
    // Then...
    assertThat( "Simple encoding", encoded, is( "123.45"));

    // Given...
    param =
      param( "myParam")
      .location( PATH)
      .style( "simple")
      .decimalData( new BigDecimal( "123.45"))
      .exploded()
      .build();

    // When...
    encoded = TestWriterUtils.getPathParameterValue( param, false);
    
    // Then...
    assertThat( "Simple encoding", encoded, is( "123.45"));
    }
  
  @Test
  public void whenSimpleUndefined()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( PATH)
      .style( "simple")
      .build()
      ;

    // When...
    String encoded = TestWriterUtils.getPathParameterValue( param);
    
    // Then...
    assertThat( "Simple encoding", encoded, is( ""));
    }
  
  @Test
  public void whenSimpleNull()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( PATH)
      .style( "simple")
      .nullData()
      .build();

    // When...
    String encoded = TestWriterUtils.getPathParameterValue( param, false);
    
    // Then...
    assertThat( "Simple encoding", encoded, is( ""));

    // Given...
    param =
      param( "myParam")
      .location( PATH)
      .style( "simple")
      .nullData()
      .exploded()
      .build();

    // When...
    encoded = TestWriterUtils.getPathParameterValue( param);
    
    // Then...
    assertThat( "Simple encoding", encoded, is( ""));
    }
  
  @Test
  public void whenSimpleArray()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( PATH)
      .style( "simple")
      .arrayData( stringOf( "A"), stringOf( "B"))
      .build();

    // When...
    String encoded = TestWriterUtils.getPathParameterValue( param, false);
    
    // Then...
    assertThat( "Simple encoding", encoded, is( "A,B"));

    // Given...
    param =
      param( "myParam")
      .location( PATH)
      .style( "simple")
      .arrayData( stringOf( "A"), stringOf( "B C"))
      .exploded()
      .build();

    // When...
    encoded = TestWriterUtils.getPathParameterValue( param);
    
    // Then...
    assertThat( "Simple encoding", encoded, is( "A,B C"));

    // When...
    encoded = TestWriterUtils.getPathParameterValue( param, true);
    
    // Then...
    assertThat( "Simple encoding", encoded, is( "A,B%20C"));
    }
  
  @Test
  public void whenSimpleObject()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( PATH)
      .style( "simple")
      .objectData( object().with( "nick name", stringOf( "X")).with( "sex", stringOf( "?")))
      .build();

    // When...
    String encoded = TestWriterUtils.getPathParameterValue( param, false);
    
    // Then...
    assertThat( "Simple encoding", encoded, is( "nick name,X,sex,?"));

    // Given...
    param =
      param( "myParam")
      .location( PATH)
      .style( "simple")
      .objectData( object().with( "nick name", stringOf( "X")).with( "sex", stringOf( "?")))
      .exploded()
      .build();

    // When...
    encoded = TestWriterUtils.getPathParameterValue( param);
    
    // Then...
    assertThat( "Simple encoding", encoded, is( "nick name=X,sex=?"));

    // When...
    encoded = TestWriterUtils.getPathParameterValue( param, true);
    
    // Then...
    assertThat( "Simple encoding", encoded, is( "nick%20name=X,sex=%3F"));
    }

  }
