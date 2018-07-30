package org.cornutum.tcases.io;

import org.junit.Test;

import static org.junit.Assert.*;

import java.math.BigDecimal;

public class ObjectUtilsTest
  {
  @Test
  public void toObject_whenNull()
    {
    // Given...
    String value = null;
    
    // When...
    Object object = ObjectUtils.toObject( value);
    
    // Then...
    assertEquals( "Null", null, object);

    // Given...
    value = "null";
    
    // When...
    object = ObjectUtils.toObject( value);
    
    // Then...
    assertEquals( "Null", null, object);
    }

  @Test
  public void toObject_whenString()
    {
    // Given...
    String value = "This is just a plain old string";
    
    // When...
    Object object = ObjectUtils.toObject( value);
    
    // Then...
    assertEquals( "String", value, object);
    }

  @Test
  public void toObject_whenInteger()
    {
    // Given...
    String value = String.valueOf( Integer.MAX_VALUE);
    
    // When...
    Object object = ObjectUtils.toObject( value);
    
    // Then...
    assertEquals( "Integer", Integer.MAX_VALUE, object);
    }

  @Test
  public void toObject_whenLong()
    {
    // Given...
    String value = String.valueOf( Long.MIN_VALUE);
    
    // When...
    Object object = ObjectUtils.toObject( value);
    
    // Then...
    assertEquals( "Long", Long.MIN_VALUE, object);
    }

  @Test
  public void toObject_whenDecimal()
    {
    // Given...
    BigDecimal decimal = new BigDecimal( "-42.00");
    String value = String.valueOf( decimal);
    
    // When...
    Object object = ObjectUtils.toObject( value);
    
    // Then...
    assertEquals( "Long", decimal, object);
    }

  @Test
  public void toObject_whenBoolean()
    {
    // Given...
    String value = "TRUE";
    
    // When...
    Object object = ObjectUtils.toObject( value);
    
    // Then...
    assertEquals( "Boolean", Boolean.TRUE, object);

    // Given...
    value = "false";
    
    // When...
    object = ObjectUtils.toObject( value);
    
    // Then...
    assertEquals( "Boolean", Boolean.FALSE, object);
    }
  }
