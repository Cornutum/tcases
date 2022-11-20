//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.*;
import static org.cornutum.tcases.resolve.DataValues.*;

import org.junit.Test;
import static org.cornutum.hamcrest.Composites.*;
import static org.hamcrest.MatcherAssert.*;

/**
 * Runs tests for {@link Schema#merge}
 */
public class MergeSchemaTest
  {
  @Test
  public void whenNumbers()
    {
    // Given...
    Schema defaults =
      SchemaBuilder.type( "number")
      .format( "double")
      .minimum( bigDecimalNull())
      .maximum( "123.4")
      .exclusiveMinimum( bigDecimalNull())
      .exclusiveMaximum( bigDecimalNull())
      .multipleOf( "0.2")
      .build();

    Schema other =
      SchemaBuilder.type( "number")
      .format( "float")
      .minimum( "-123.4")
      .maximum( bigDecimalNull())
      .exclusiveMinimum( bigDecimalNull())
      .exclusiveMaximum( "124.0")
      .multipleOf( "1.2")
      .build();
        
    // When...
    Schema merged = defaults.merge( other);
    
    // Then...
    Schema expected =
      SchemaBuilder.type( "number")
      .format( "float")
      .minimum( "-123.4")
      .maximum( "123.4")
      .exclusiveMinimum( bigDecimalNull())
      .exclusiveMaximum( "124.0")
      .multipleOf( "1.2")
      .build();
    
    assertThat( "Merged", merged, matches( new SchemaMatcher( expected)));
    assertThat( "Copied", SchemaBuilder.with( merged).build(), matches( new SchemaMatcher( expected)));
    }
  
  @Test
  public void whenIntegers()
    {
    // Given...
    Schema defaults =
      SchemaBuilder.type( "integer")
      .format( null)
      .minimum( 0)
      .maximum( bigDecimalNull())
      .exclusiveMinimum( bigDecimalNull())
      .exclusiveMaximum( 100)
      .multipleOf( bigDecimalNull())
      .build();

    Schema other =
      SchemaBuilder.type( "integer")
      .format( "int64")
      .minimum( 1)
      .maximum( 16)
      .exclusiveMinimum( 0)
      .exclusiveMaximum( bigDecimalNull())
      .multipleOf( bigDecimalNull())
      .build();
        
    // When...
    Schema merged = defaults.merge( other);
    
    // Then...
    Schema expected =
      SchemaBuilder.type( "integer")
      .format( "int64")
      .minimum( 1)
      .maximum( 16)
      .exclusiveMinimum( 0)
      .exclusiveMaximum( 100)
      .multipleOf( bigDecimalNull())
      .build();
    
    assertThat( "Merged", merged, matches( new SchemaMatcher( expected)));
    assertThat( "Copied", SchemaBuilder.with( merged).build(), matches( new SchemaMatcher( expected)));
    }
  
  @Test
  public void whenNumberInteger()
    {
    // Given...
    Schema defaults =
      SchemaBuilder.type( "number")
      .format( "double")
      .minimum( bigDecimalNull())
      .maximum( "1.234")
      .exclusiveMinimum( "1.234")
      .exclusiveMaximum( "2.345")
      .multipleOf( "0.001")
      .build();

    Schema other =
      SchemaBuilder.type( "integer")
      .format( null)
      .minimum( bigDecimalNull())
      .maximum( 100)
      .exclusiveMinimum( 0)
      .exclusiveMaximum( bigDecimalNull())
      .multipleOf( 1)
      .build();
        
    // When...
    Schema merged = defaults.merge( other);
    
    // Then...
    Schema expected =
      SchemaBuilder.type( "integer")
      .format( null)
      .minimum( bigDecimalNull())
      .maximum( 100)
      .exclusiveMinimum( 0)
      .exclusiveMaximum( 3)
      .multipleOf( 1)
      .build();
    
    assertThat( "Merged", merged, matches( new SchemaMatcher( expected)));
    assertThat( "Copied", SchemaBuilder.with( merged).build(), matches( new SchemaMatcher( expected)));
    }
  
  @Test
  public void whenIntegerNumber()
    {
    // Given...
    Schema defaults =
      SchemaBuilder.type( "integer")
      .format( "null")
      .minimum( 0)
      .maximum( 100)
      .exclusiveMinimum( -1)
      .exclusiveMaximum( 101)
      .multipleOf( 10)
      .build();

    Schema other =
      SchemaBuilder.type( "number")
      .format( "null")
      .minimum( bigDecimalNull())
      .maximum( bigDecimalNull())
      .exclusiveMinimum( bigDecimalNull())
      .exclusiveMaximum( "123.45")
      .multipleOf( "0.05")
      .build();
        
    // When...
    Schema merged = defaults.merge( other);
    
    // Then...
    Schema expected =
      SchemaBuilder.type( "number")
      .format( "null")
      .minimum( 0)
      .maximum( 100)
      .exclusiveMinimum( -1)
      .exclusiveMaximum( "123.45")
      .multipleOf( "0.05")
      .build();
    
    assertThat( "Merged", merged, matches( new SchemaMatcher( expected)));
    assertThat( "Copied", SchemaBuilder.with( merged).build(), matches( new SchemaMatcher( expected)));
    }
  
  @Test
  public void whenArraysUniqueItemsNone()
    {
    // Given...
    Schema defaults =
      SchemaBuilder.type( "array")
      .format( "list")
      .minItems( null)
      .maxItems( 8)
      .uniqueItems( null)
      .items( SchemaBuilder.type( "integer").build())
      .build();

    Schema other =
      SchemaBuilder.type( "array")
      .format( "base64")
      .minItems( 0)
      .maxItems( null)
      .uniqueItems( null)
      .items( SchemaBuilder.type( "string").build())
      .build();
        
    // When...
    Schema merged = defaults.merge( other);
    
    // Then...
    Schema expected =
      SchemaBuilder.type( "array")
      .format( "base64")
      .minItems( 0)
      .maxItems( 8)
      .uniqueItems( null)
      .items( SchemaBuilder.type( "string").build())
      .build();
    
    assertThat( "Merged", merged, matches( new SchemaMatcher( expected)));
    assertThat( "Copied", SchemaBuilder.with( merged).build(), matches( new SchemaMatcher( expected)));
    }
  
  @Test
  public void whenArraysOverride()
    {
    // Given...
    Schema defaults =
      SchemaBuilder.type( "array")
      .format( null)
      .minItems( 1)
      .maxItems( null)
      .uniqueItems( false)
      .items( null)
      .build();

    Schema other =
      SchemaBuilder.type( "array")
      .format( "list")
      .minItems( 3)
      .maxItems( 3)
      .uniqueItems( null)
      .items( SchemaBuilder.type( "integer").build())
      .build();
        
    // When...
    Schema merged = defaults.merge( other);
    
    // Then...
    Schema expected =
      SchemaBuilder.type( "array")
      .format( "list")
      .minItems( 3)
      .maxItems( 3)
      .uniqueItems( false)
      .items( SchemaBuilder.type( "integer").build())
      .build();
    
    assertThat( "Merged", merged, matches( new SchemaMatcher( expected)));
    assertThat( "Copied", SchemaBuilder.with( merged).build(), matches( new SchemaMatcher( expected)));
    }
  
  @Test
  public void whenArraysDefaults()
    {
    // Given...
    Schema defaults =
      SchemaBuilder.type( "array")
      .format( "base64")
      .minItems( 2)
      .maxItems( null)
      .items( SchemaBuilder.type( "number").build())
      .uniqueItems( null)
      .build();

    Schema other =
      SchemaBuilder.type( "array")
      .format( null)
      .minItems( null)
      .maxItems( null)
      .items( null)
      .uniqueItems( true)
      .build();
        
    // When...
    Schema merged = defaults.merge( other);
    
    // Then...
    Schema expected =
      SchemaBuilder.type( "array")
      .format( "base64")
      .minItems( 2)
      .maxItems( null)
      .items( SchemaBuilder.type( "number").build())
      .uniqueItems( true)
      .build();
    
    assertThat( "Merged", merged, matches( new SchemaMatcher( expected)));
    assertThat( "Copied", SchemaBuilder.with( merged).build(), matches( new SchemaMatcher( expected)));
    }
  
  @Test
  public void whenArraysSchemaNone()
    {
    // Given...
    Schema defaults =
      SchemaBuilder.type( "array")
      .format( null)
      .minItems( null)
      .maxItems( 8)
      .uniqueItems( true)
      .items( null)
      .build();

    Schema other =
      SchemaBuilder.type( "array")
      .format( null)
      .minItems( null)
      .maxItems( 16)
      .uniqueItems( false)
      .items( null)
      .build();
        
    // When...
    Schema merged = defaults.merge( other);
    
    // Then...
    Schema expected =
      SchemaBuilder.type( "array")
      .format( null)
      .minItems( null)
      .maxItems( 16)
      .uniqueItems( false)
      .items( null)
      .build();
    
    assertThat( "Merged", merged, matches( new SchemaMatcher( expected)));
    assertThat( "Copied", SchemaBuilder.with( merged).build(), matches( new SchemaMatcher( expected)));
    }
  
  @Test
  public void whenStringsMinLengthNone()
    {
    // Given...
    Schema defaults =
      SchemaBuilder.type( "string")
      .format( "email")
      .minLength( null)
      .maxLength( 128)
      .pattern( null)
      .build();

    Schema other =
      SchemaBuilder.type( "string")
      .format( "uuid")
      .minLength( null)
      .maxLength( null)
      .pattern( "[0-9]")
      .build();
        
    // When...
    Schema merged = defaults.merge( other);
    
    // Then...
    Schema expected =
      SchemaBuilder.type( "string")
      .format( "uuid")
      .minLength( null)
      .maxLength( 128)
      .pattern( "[0-9]")
      .build();
    
    assertThat( "Merged", merged, matches( new SchemaMatcher( expected)));
    assertThat( "Copied", SchemaBuilder.with( merged).build(), matches( new SchemaMatcher( expected)));
    }
  
  @Test
  public void whenStringsMaxLengthNone()
    {
    // Given...
    Schema defaults =
      SchemaBuilder.type( "string")
      .format( null)
      .minLength( null)
      .maxLength( null)
      .pattern( "[A-Z]")
      .build();

    Schema other =
      SchemaBuilder.type( "string")
      .format( "date")
      .minLength( 0)
      .maxLength( null)
      .pattern( ".")
      .build();
        
    // When...
    Schema merged = defaults.merge( other);
    
    // Then...
    Schema expected =
      SchemaBuilder.type( "string")
      .format( "date")
      .minLength( 0)
      .maxLength( null)
      .pattern( ".")
      .build();
    
    assertThat( "Merged", merged, matches( new SchemaMatcher( expected)));
    assertThat( "Copied", SchemaBuilder.with( merged).build(), matches( new SchemaMatcher( expected)));
    }
  
  @Test
  public void whenStringsPatternNone()
    {
    // Given...
    Schema defaults =
      SchemaBuilder.type( "string")
      .format( "date-time")
      .minLength( 1)
      .maxLength( null)
      .pattern( null)
      .build();

    Schema other =
      SchemaBuilder.type( "string")
      .format( null)
      .minLength( 16)
      .maxLength( 128)
      .pattern( null)
      .build();
        
    // When...
    Schema merged = defaults.merge( other);
    
    // Then...
    Schema expected =
      SchemaBuilder.type( "string")
      .format( "date-time")
      .minLength( 16)
      .maxLength( 128)
      .pattern( null)
      .build();
    
    assertThat( "Merged", merged, matches( new SchemaMatcher( expected)));
    assertThat( "Copied", SchemaBuilder.with( merged).build(), matches( new SchemaMatcher( expected)));
    }
  
  @Test
  public void whenStringsFormatNone()
    {
    // Given...
    Schema defaults =
      SchemaBuilder.type( "string")
      .format( null)
      .minLength( 1)
      .maxLength( 128)
      .pattern( "[a-z]")
      .build();

    Schema other =
      SchemaBuilder.type( "string")
      .format( null)
      .minLength( null)
      .maxLength( 256)
      .pattern( null)
      .build();
        
    // When...
    Schema merged = defaults.merge( other);
    
    // Then...
    Schema expected =
      SchemaBuilder.type( "string")
      .format( null)
      .minLength( 1)
      .maxLength( 256)
      .pattern( "[a-z]")
      .build();
    
    assertThat( "Merged", merged, matches( new SchemaMatcher( expected)));
    assertThat( "Copied", SchemaBuilder.with( merged).build(), matches( new SchemaMatcher( expected)));
    }
  
  @Test
  public void whenBoolean()
    {
    // Given...
    Schema defaults =
      SchemaBuilder.type( "boolean")
      .format( "string")
      .build();

    Schema other =
      SchemaBuilder.generic()
      .constant( true)
      .build();
        
    // When...
    Schema merged = defaults.merge( other);
    
    // Then...
    Schema expected =
      SchemaBuilder.type( "boolean")
      .format( "string")
      .constant( true)
      .build();
    
    assertThat( "Merged", merged, matches( new SchemaMatcher( expected)));
    assertThat( "Copied", SchemaBuilder.with( merged).build(), matches( new SchemaMatcher( expected)));
    }
  
  @Test
  public void whenNullFormatOverride()
    {
    // Given...
    Schema defaults =
      SchemaBuilder.generic()
      .format( "int16")
      .build();

    Schema other =
      SchemaBuilder.generic()
      .format( "int64")
      .build();
        
    // When...
    Schema merged = defaults.merge( other);
    
    // Then...
    Schema expected =
      SchemaBuilder.generic()
      .format( "int64")
      .build();
    
    assertThat( "Merged", merged, matches( new SchemaMatcher( expected)));
    assertThat( "Copied", SchemaBuilder.with( merged).build(), matches( new SchemaMatcher( expected)));
    }
  
  @Test
  public void whenNullValueDefault()
    {
    // Given...
    Schema defaults =
      SchemaBuilder.generic()
      .format( null)
      .constant( nullValue())
      .build();

    Schema other =
      SchemaBuilder.generic()
      .format( "email")
      .build();
        
    // When...
    Schema merged = defaults.merge( other);
    
    // Then...
    Schema expected =
      SchemaBuilder.generic()
      .format( "email")
      .constant( nullValue())
      .build();
    
    assertThat( "Merged", merged, matches( new SchemaMatcher( expected)));
    assertThat( "Copied", SchemaBuilder.with( merged).build(), matches( new SchemaMatcher( expected)));
    }
  
  @Test
  public void whenNullFormatDefault()
    {
    // Given...
    Schema defaults =
      SchemaBuilder.generic()
      .format( "uuid")
      .constant( nullValue())
      .build();

    Schema other =
      SchemaBuilder.generic()
      .format( null)
      .constant( nullValue())
      .build();
        
    // When...
    Schema merged = defaults.merge( other);
    
    // Then...
    Schema expected =
      SchemaBuilder.generic()
      .format( "uuid")
      .constant( nullValue())
      .build();
    
    assertThat( "Merged", merged, matches( new SchemaMatcher( expected)));
    assertThat( "Copied", SchemaBuilder.with( merged).build(), matches( new SchemaMatcher( expected)));
    }
  
  @Test
  public void whenDefaultConstant()
    {
    // Given...
    Schema defaults =
      SchemaBuilder.generic()
      .format( "uuid")
      .constant( nullValue())
      .build();

    Schema other =
      SchemaBuilder.type( "number")
      .format( null)
      .minimum( "-123.4")
      .maximum( bigDecimalNull())
      .exclusiveMinimum( bigDecimalNull())
      .exclusiveMaximum( "124.0")
      .multipleOf( "1.2")
      .build();
        
    // When...
    Schema merged = defaults.merge( other);
    
    // Then...
    Schema expected = other;
    
    assertThat( "Merged", merged, matches( new SchemaMatcher( expected)));
    assertThat( "Copied", SchemaBuilder.with( merged).build(), matches( new SchemaMatcher( expected)));
    }
  
  @Test
  public void whenOtherConstant()
    {
    // Given...
    Schema defaults =
      SchemaBuilder.type( "string")
      .format( "email")
      .minLength( null)
      .maxLength( 128)
      .pattern( null)
      .build();

    Schema other =
      SchemaBuilder.type( "string")
      .constant( "me@my.org")
      .build();
        
    // When...
    Schema merged = defaults.merge( other);
    
    // Then...
    Schema expected =
      SchemaBuilder.type( "string")
      .format( "email")
      .constant( "me@my.org")
      .build();
    
    assertThat( "Merged", merged, matches( new SchemaMatcher( expected)));
    assertThat( "Copied", SchemaBuilder.with( merged).build(), matches( new SchemaMatcher( expected)));
    }
  }
