package org.cornutum.tcases.io;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

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
    assertThat( "Null", object, is( (Object) null));

    // Given...
    value = "null";
    
    // When...
    object = ObjectUtils.toObject( value);
    
    // Then...
    assertThat( "Null", object, is( (Object) null));
    }

  @Test
  public void toObject_whenString()
    {
    // Given...
    String value = "This is just a plain old string";
    
    // When...
    Object object = ObjectUtils.toObject( value);
    
    // Then...
    assertThat( "String", object, is( value));
    }

  @Test
  public void toObject_whenInteger()
    {
    // Given...
    String value = String.valueOf( Integer.MAX_VALUE);
    
    // When...
    Object object = ObjectUtils.toObject( value);
    
    // Then...
    assertThat( "Integer", object, is( Integer.MAX_VALUE));
    }

  @Test
  public void toObject_whenLong()
    {
    // Given...
    String value = String.valueOf( Long.MIN_VALUE);
    
    // When...
    Object object = ObjectUtils.toObject( value);
    
    // Then...
    assertThat( "Long", object, is( Long.MIN_VALUE));
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
    assertThat( "Long", object, is( decimal));
    }

  @Test
  public void toObject_whenBoolean()
    {
    // Given...
    String value = "TRUE";
    
    // When...
    Object object = ObjectUtils.toObject( value);
    
    // Then...
    assertThat( "Boolean", object, is( Boolean.TRUE));

    // Given...
    value = "false";
    
    // When...
    object = ObjectUtils.toObject( value);
    
    // Then...
    assertThat( "Boolean", object, is( Boolean.FALSE));
    }
  }
