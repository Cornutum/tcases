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
 * Runs tests for {@link TestWriterUtils.MatrixValueEncoder}.
 */
@SuppressWarnings("unchecked")
public class MatrixValueEncoderTest
  {  
  @Test
  public void whenMatrixDecimal()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( PATH)
      .style( "matrix")
      .decimalData( new BigDecimal( "123.45"))
      .build();

    // When...
    String encoded = TestWriterUtils.getPathParameterValue( param);
    
    // Then...
    assertThat( "Matrix encoding", encoded, is( ";myParam=123.45"));

    // Given...
    param =
      param( "myParam")
      .location( PATH)
      .style( "matrix")
      .decimalData( new BigDecimal( "123.45"))
      .exploded()
      .build();

    // When...
    encoded = TestWriterUtils.getPathParameterValue( param);
    
    // Then...
    assertThat( "Matrix encoding", encoded, is( ";myParam=123.45"));
    }
  
  @Test
  public void whenMatrixUndefined()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( PATH)
      .style( "matrix")
      .build()
      ;

    // When...
    String encoded = TestWriterUtils.getPathParameterValue( param);
    
    // Then...
    assertThat( "Matrix encoding", encoded, is( ""));
    }
  
  @Test
  public void whenMatrixNull()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( PATH)
      .style( "matrix")
      .nullData()
      .build();

    // When...
    String encoded = TestWriterUtils.getPathParameterValue( param);
    
    // Then...
    assertThat( "Matrix encoding", encoded, is( ";myParam"));

    // Given...
    param =
      param( "myParam")
      .location( PATH)
      .style( "matrix")
      .nullData()
      .exploded()
      .build();

    // When...
    encoded = TestWriterUtils.getPathParameterValue( param);
    
    // Then...
    assertThat( "Matrix encoding", encoded, is( ";myParam"));
    }
  
  @Test
  public void whenMatrixArray()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( PATH)
      .style( "matrix")
      .arrayData( stringOf( "A"), stringOf( "B"))
      .build();

    // When...
    String encoded = TestWriterUtils.getPathParameterValue( param);
    
    // Then...
    assertThat( "Matrix encoding", encoded, is( ";myParam=A,B"));

    // Given...
    param =
      param( "myParam")
      .location( PATH)
      .style( "matrix")
      .arrayData( stringOf( "A"), stringOf( "B"))
      .exploded()
      .build();

    // When...
    encoded = TestWriterUtils.getPathParameterValue( param);
    
    // Then...
    assertThat( "Matrix encoding", encoded, is( ";myParam=A;myParam=B"));
    }
  
  @Test
  public void whenMatrixObject()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( PATH)
      .style( "matrix")
      .objectData( object().with( "name", stringOf( "X")).with( "sex", valueOf( true)))
      .build();

    // When...
    String encoded = TestWriterUtils.getPathParameterValue( param);
    
    // Then...
    assertThat( "Matrix encoding", encoded, is( ";myParam=name,X,sex,true"));

    // Given...
    param =
      param( "myParam")
      .location( PATH)
      .style( "matrix")
      .objectData( object().with( "name", stringOf( "X")).with( "sex", valueOf( true)))
      .exploded()
      .build();

    // When...
    encoded = TestWriterUtils.getPathParameterValue( param);
    
    // Then...
    assertThat( "Matrix encoding", encoded, is( ";name=X;sex=true"));
    }

  }
