//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter.encoder;

import org.cornutum.tcases.openapi.resolver.ParamData;
import org.cornutum.tcases.openapi.testwriter.RequestCaseUtils;

import static org.cornutum.tcases.openapi.resolver.ParamDataBuilder.param;
import static org.cornutum.tcases.openapi.resolver.ParamDef.Location.*;
import static org.cornutum.tcases.resolve.DataValues.*;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.math.BigDecimal;

/**
 * Runs tests for {@link MatrixValueEncoder}.
 */
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
    String encoded = RequestCaseUtils.getPathParameterValue( param);
    
    // Then...
    assertThat( "Matrix encoding", encoded, is( ";myParam=123.45"));

    // Given...
    param =
      param( "my Param")
      .location( PATH)
      .style( "matrix")
      .decimalData( new BigDecimal( "123.45"))
      .exploded()
      .build();

    // When...
    encoded = RequestCaseUtils.getPathParameterValue( param, false);
    
    // Then...
    assertThat( "Matrix encoding", encoded, is( ";my Param=123.45"));

    // When...
    encoded = RequestCaseUtils.getPathParameterValue( param, true);
    
    // Then...
    assertThat( "Matrix encoding", encoded, is( ";my%20Param=123.45"));
    }
  
  @Test
  public void whenMatrixString()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( PATH)
      .style( "matrix")
      .stringData( "X[Y]")
      .build();

    // When...
    String encoded = RequestCaseUtils.getPathParameterValue( param);
    
    // Then...
    assertThat( "Matrix encoding", encoded, is( ";myParam=X[Y]"));

    // Given...
    param =
      param( "myParam")
      .location( PATH)
      .style( "matrix")
      .stringData( "X[Y]")
      .exploded()
      .build();

    // When...
    encoded = RequestCaseUtils.getPathParameterValue( param, true);
    
    // Then...
    assertThat( "Matrix encoding", encoded, is( ";myParam=X%5BY%5D"));

    // Given...
    param =
      param( "myParam")
      .location( PATH)
      .style( "matrix")
      .stringData( "")
      .exploded()
      .build();

    // When...
    encoded = RequestCaseUtils.getPathParameterValue( param, true);
    
    // Then...
    assertThat( "Matrix encoding", encoded, is( ";myParam"));
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
    String encoded = RequestCaseUtils.getPathParameterValue( param);
    
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
    String encoded = RequestCaseUtils.getPathParameterValue( param, true);
    
    // Then...
    assertThat( "Matrix encoding", encoded, is( ""));

    // Given...
    param =
      param( "myParam")
      .location( PATH)
      .style( "matrix")
      .nullData()
      .exploded()
      .build();

    // When...
    encoded = RequestCaseUtils.getPathParameterValue( param);
    
    // Then...
    assertThat( "Matrix encoding", encoded, is( ""));
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
    String encoded = RequestCaseUtils.getPathParameterValue( param, false);
    
    // Then...
    assertThat( "Matrix encoding", encoded, is( ";myParam=A,B"));

    // Given...
    param =
      param( "myParam")
      .location( PATH)
      .style( "matrix")
      .arrayData( stringOf( "A"), stringOf( "B C"))
      .exploded()
      .build();

    // When...
    encoded = RequestCaseUtils.getPathParameterValue( param);
    
    // Then...
    assertThat( "Matrix encoding", encoded, is( ";myParam=A;myParam=B C"));

    // When...
    encoded = RequestCaseUtils.getPathParameterValue( param, true);
    
    // Then...
    assertThat( "Matrix encoding", encoded, is( ";myParam=A;myParam=B%20C"));
    }
  
  @Test
  public void whenMatrixArrayEmpty()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( PATH)
      .style( "matrix")
      .arrayData()
      .build();

    // When...
    String encoded = RequestCaseUtils.getPathParameterValue( param, false);
    
    // Then...
    assertThat( "Matrix encoding", encoded, is( ";myParam"));

    // Given...
    param =
      param( "myParam")
      .location( PATH)
      .style( "matrix")
      .arrayData()
      .exploded()
      .build();

    // When...
    encoded = RequestCaseUtils.getPathParameterValue( param);
    
    // Then...
    assertThat( "Matrix encoding", encoded, is( ";myParam"));

    // When...
    encoded = RequestCaseUtils.getPathParameterValue( param, true);
    
    // Then...
    assertThat( "Matrix encoding", encoded, is( ";myParam"));
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
    String encoded = RequestCaseUtils.getPathParameterValue( param, false);
    
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
    encoded = RequestCaseUtils.getPathParameterValue( param);
    
    // Then...
    assertThat( "Matrix encoding", encoded, is( ";name=X;sex=true"));
    }

  }
