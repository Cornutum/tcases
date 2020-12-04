//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter.encoder;

import org.cornutum.tcases.openapi.resolver.Base64Domain;
import org.cornutum.tcases.openapi.resolver.DataValue;
import org.cornutum.tcases.util.ListBuilder;
import static org.cornutum.tcases.openapi.resolver.DataValues.*;

import org.junit.Test;
import static org.cornutum.hamcrest.Composites.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.is;

import java.math.BigDecimal;
import java.util.AbstractMap.SimpleEntry;
import java.util.List;
import java.util.Map;
import static java.util.Collections.emptyList;
  
/**
 * Runs tests for {@link FormUrlEncoder}
 */
public class FormUrlEncoderTest
  {
  @Test
  public void whenArrayValue()
    {
    // Given...
    DataValue<?> value =
      arrayOf(
        arrayOf( valueOf(1), valueOf(2), valueOf(3)),
        arrayOf( valueOf(4), valueOf(5)),
        arrayOf( valueOf(6)));
    
    // When...
    List<Map.Entry<String,String>> bindings = FormUrlEncoder.encode( value);
    
    // Then...
    assertThat(
      "Array",
      bindings,
      listsMembers(
        form()
        .with( "0", "1%2C2%2C3")
        .with( "1", "4%2C5")
        .with( "2", "6")
        .build()));

    // When...
    String content = FormUrlEncoder.toForm( value);

    // Then...
    assertThat( "Content", content, is( "0=1%2C2%2C3&1=4%2C5&2=6"));
    }

  @Test
  public void whenBinaryValue()
    {
    // Given...
    byte[] bytes = new byte[]{-10, -1, 1, 10};
    DataValue<?> value = valueOf( bytes);
    
    // When...
    List<Map.Entry<String,String>> bindings = FormUrlEncoder.encode( value, false);
    
    // Then...
    assertThat(
      "Binary",
      bindings,
      listsMembers(
        form()
        .with( "bytes", Base64Domain.encoded( bytes))
        .build()));
    }

  @Test
  public void whenBooleanValue()
    {
    // Given...
    DataValue<?> value = valueOf( false);
    
    // When...
    List<Map.Entry<String,String>> bindings = FormUrlEncoder.encode( value);
    
    // Then...
    assertThat(
      "Boolean",
      bindings,
      listsMembers(
        form()
        .with( "boolean", "false")
        .build()));
    }

  @Test
  public void whenDecimalValue()
    {
    // Given...
    DataValue<?> value = valueOf( new BigDecimal( "123.456"));
    
    // When...
    List<Map.Entry<String,String>> bindings = FormUrlEncoder.encode( value);
    
    // Then...
    assertThat(
      "Decimal",
      bindings,
      listsMembers(
        form()
        .with( "number", "123.456")
        .build()));
    }

  @Test
  public void whenIntegerValue()
    {
    // Given...
    DataValue<?> value = valueOf( -12345);
    
    // When...
    List<Map.Entry<String,String>> bindings = FormUrlEncoder.encode( value);
    
    // Then...
    assertThat(
      "Integer",
      bindings,
      listsMembers(
        form()
        .with( "integer", "-12345")
        .build()));
    }

  @Test
  public void whenLongValue()
    {
    // Given...
    DataValue<?> value = valueOf( Long.MIN_VALUE);
    
    // When...
    List<Map.Entry<String,String>> bindings = FormUrlEncoder.encode( value);
    
    // Then...
    assertThat(
      "Long",
      bindings,
      listsMembers(
        form()
        .with( "integer", String.valueOf( Long.MIN_VALUE))
        .build()));
    }

  @Test
  public void whenNullValue()
    {
    // Given...
    DataValue<?> value = nullValue();
    
    // When...
    List<Map.Entry<String,String>> bindings = FormUrlEncoder.encode( value);
    
    // Then...
    assertThat(
      "Null",
      bindings,
      is( emptyList()));
    }

  @Test
  public void whenNoValue()
    {
    // Given...
    DataValue<?> value = null;
    
    // When...
    List<Map.Entry<String,String>> bindings = FormUrlEncoder.encode( value);
    
    // Then...
    assertThat(
      "Undefined",
      bindings,
      is( emptyList()));
    }

  @Test
  public void whenObjectValue()
    {
    // Given...
    DataValue<?> value =
      object()
      .with( "What?", object().with( "Me?", stringOf( "Worry?")).build())
      .with( "Some Numbers", arrayOf( valueOf(-1), valueOf( 0), valueOf(1)))
      .with( "Nothing", nullValue())
      .build();
    
    // When...
    List<Map.Entry<String,String>> bindings = FormUrlEncoder.encode( value);
    
    // Then...
    assertThat(
      "Object",
      bindings,
      listsMembers(
        form()
        .with( "What%3F", "Me%3F%2CWorry%3F")
        .with( "Some+Numbers", "-1%2C0%2C1")
        .with( "Nothing", "")
        .build()));
    }

  @Test
  public void whenStringValue()
    {
    // Given...
    DataValue<?> value = stringOf( "What? Me, worry? E=mc&2");
    
    // When...
    List<Map.Entry<String,String>> bindings = FormUrlEncoder.encode( value);
    
    // Then...
    assertThat(
      "String",
      bindings,
      listsMembers(
        form()
        .with( "string", "What%3F+Me%2C+worry%3F+E%3Dmc%262")
        .build()));
    }

  private BindingsBuilder form()
    {
    return new BindingsBuilder();
    }
  
  private static class BindingsBuilder extends ListBuilder<Map.Entry<String,String>>
    {
    private BindingsBuilder()
      {
      }
    
    public BindingsBuilder with( String name, String value)
      {
      add( new SimpleEntry<String,String>(name, value));
      return this;
      }
    }
  }
