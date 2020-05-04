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
 * Runs tests for {@link TestWriterUtils.LabelValueEncoder}.
 */
@SuppressWarnings("unchecked")
public class LabelValueEncoderTest
  {  
  @Test
  public void whenLabelDecimal()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( PATH)
      .style( "label")
      .decimalData( new BigDecimal( "123.45"))
      .build();

    // When...
    String encoded = TestWriterUtils.getPathParameterValue( param);
    
    // Then...
    assertThat( "Label encoding", encoded, is( ".123.45"));

    // Given...
    param =
      param( "myParam")
      .location( PATH)
      .style( "label")
      .decimalData( new BigDecimal( "123.45"))
      .exploded()
      .build();

    // When...
    encoded = TestWriterUtils.getPathParameterValue( param, false);
    
    // Then...
    assertThat( "Label encoding", encoded, is( ".123.45"));
    }
  
  @Test
  public void whenLabelUndefined()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( PATH)
      .style( "label")
      .build()
      ;

    // When...
    String encoded = TestWriterUtils.getPathParameterValue( param);
    
    // Then...
    assertThat( "Label encoding", encoded, is( ""));
    }
  
  @Test
  public void whenLabelNull()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( PATH)
      .style( "label")
      .nullData()
      .build();

    // When...
    String encoded = TestWriterUtils.getPathParameterValue( param);
    
    // Then...
    assertThat( "Label encoding", encoded, is( "."));

    // Given...
    param =
      param( "myParam")
      .location( PATH)
      .style( "label")
      .nullData()
      .exploded()
      .build();

    // When...
    encoded = TestWriterUtils.getPathParameterValue( param, false);
    
    // Then...
    assertThat( "Label encoding", encoded, is( "."));
    }
  
  @Test
  public void whenLabelArray()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( PATH)
      .style( "label")
      .arrayData( stringOf( "A"), stringOf( "B"))
      .build();

    // When...
    String encoded = TestWriterUtils.getPathParameterValue( param);
    
    // Then...
    assertThat( "Label encoding", encoded, is( ".A.B"));

    // Given...
    param =
      param( "myParam")
      .location( PATH)
      .style( "label")
      .arrayData( stringOf( "A"), stringOf( "B C"))
      .exploded()
      .build();

    // When...
    encoded = TestWriterUtils.getPathParameterValue( param, false);
    
    // Then...
    assertThat( "Label encoding", encoded, is( ".A.B C"));

    // When...
    encoded = TestWriterUtils.getPathParameterValue( param, true);
    
    // Then...
    assertThat( "Label encoding", encoded, is( ".A.B%20C"));
    }
  
  @Test
  public void whenLabelObject()
    {
    // Given...
    ParamData param =
      param( "myParam")
      .location( PATH)
      .style( "label")
      .objectData( object().with( "name", stringOf( "X")).with( "sex", valueOf( true)))
      .build();

    // When...
    String encoded = TestWriterUtils.getPathParameterValue( param);
    
    // Then...
    assertThat( "Label encoding", encoded, is( ".name.X.sex.true"));

    // Given...
    param =
      param( "myParam")
      .location( PATH)
      .style( "label")
      .objectData( object().with( "name", stringOf( "X")).with( "sex", valueOf( true)))
      .exploded()
      .build();

    // When...
    encoded = TestWriterUtils.getPathParameterValue( param, false);
    
    // Then...
    assertThat( "Label encoding", encoded, is( ".name=X.sex=true"));
    }

  }
