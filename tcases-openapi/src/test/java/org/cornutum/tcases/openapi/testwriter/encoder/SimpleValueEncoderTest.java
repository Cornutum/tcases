//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter.encoder;

import org.cornutum.tcases.openapi.resolver.ParamData;
import org.cornutum.tcases.openapi.testwriter.TestWriterUtils;
import org.cornutum.tcases.openapi.testwriter.encoder.SimpleValueEncoder;

import static org.cornutum.tcases.openapi.resolver.DataValues.*;
import static org.cornutum.tcases.openapi.resolver.ParamDataBuilder.param;
import static org.cornutum.tcases.openapi.resolver.ParamDef.Location.*;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.math.BigDecimal;
import java.util.Optional;

/**
 * Runs tests for {@link SimpleValueEncoder}.
 */
@SuppressWarnings("unchecked")
public class SimpleValueEncoderTest
  {  
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
  
  @Test
  public void whenHeaderValueReserved()
    {
    // Given...
    String reservedChars = "?X=Y&A=B 1|2|3";
    ParamData param =
      param( "myParam")
      .location( HEADER)
      .style( "simple")
      .stringData( reservedChars)
      .build();

    // When...
    Optional<String> value = TestWriterUtils.getHeaderParameterValue( param);

    // Then...
    assertThat( "Header value", value.orElse(null), is( reservedChars));
    }
  
  @Test
  public void whenHeaderValueQuoted()
    {
    // Given...
    String lws = " ...ends here.";
    ParamData param =
      param( "myParam")
      .location( HEADER)
      .style( "simple")
      .stringData( lws)
      .build();

    // When...
    Optional<String> value = TestWriterUtils.getHeaderParameterValue( param);

    // Then...
    assertThat( "Header value", value.orElse(null), is( String.format( "\"%s\"", lws)));
    
    // Given...
    String tws = "Starts here... ";
    param =
      param( "myParam")
      .location( HEADER)
      .style( "simple")
      .stringData( tws)
      .build();

    // When...
    value = TestWriterUtils.getHeaderParameterValue( param);

    // Then...
    assertThat( "Header value", value.orElse(null), is( String.format( "\"%s\"", tws)));

    // Given...
    String empty = "";
    param =
      param( "myParam")
      .location( HEADER)
      .style( "simple")
      .stringData( empty)
      .build();

    // When...
    value = TestWriterUtils.getHeaderParameterValue( param);

    // Then...
    assertThat( "Header value", value.orElse(null), is( empty));
    }
  
  @Test
  public void whenHeaderValueUndefined()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( HEADER)
      .style( "simple")
      .build();

    // When...
    Optional<String> value = TestWriterUtils.getHeaderParameterValue( param);

    // Then...
    assertThat( "Header value", value, is( Optional.empty()));
    }

  }
