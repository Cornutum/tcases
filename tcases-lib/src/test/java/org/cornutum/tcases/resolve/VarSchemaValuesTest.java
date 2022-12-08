//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.*;
import static org.cornutum.tcases.VarValueDef.Type.*;
import static org.cornutum.tcases.resolve.DataValues.*;

import org.junit.Test;
import static org.cornutum.hamcrest.Composites.*;
import static org.hamcrest.MatcherAssert.*;

import java.util.Arrays;
import java.util.List;
import static java.util.stream.Collectors.toList;

/**
 * Runs tests for {@link TestCaseSchemaResolver#valuesForSchema}
 */
public class VarSchemaValuesTest extends ResolverTest
  {
  @Test
  public void whenMinItemsAboveMax()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "array")
      .minItems( 10)
      .maxItems( 4)
      .uniqueItems( true)
      .build();
        
    // When...
    List<VarValueDef> values = resolver.valuesForSchema( schema).collect( toList());
    
    // Then...
    VarValueDef[] expected =
      new VarValueDef[]
      {
        VarValueDefBuilder.with( "minimumSize")
        .schema(
          SchemaBuilder.with( schema)
          .build())
        .build(),

        VarValueDefBuilder.with( "tooSmall")
        .type( FAILURE)
        .schema(
          SchemaBuilder.with( schema)
          .minItems( 0)
          .maxItems( 3)
          .build())
        .build(),

        VarValueDefBuilder.with( "tooLarge")
        .type( FAILURE)
        .schema(
          SchemaBuilder.with( schema)
          .minItems( 5)
          .maxItems( null)
          .build())
        .build(),

        VarValueDefBuilder.with( "notUnique")
        .type( FAILURE)
        .schema(
          SchemaBuilder.with( schema)
          .uniqueItems( false)
          .build())
        .build()
      };

    assertThat( "Values", values, containsMembers( VarValueDefMatcher::new, expected));
    }

  @Test
  public void whenItemsNormalized()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "array")
      .format( "set")
      .items(
        SchemaBuilder.type( "integer")
        .exclusiveMinimum( 11)
        .exclusiveMaximum( 10)
        .build())
      .build();
        
    // When...
    List<VarValueDef> values = resolver.valuesForSchema( schema).collect( toList());
    
    // Then...
    VarValueDef[] expected =
      new VarValueDef[]
      {
        VarValueDefBuilder.with( "empty")
        .schema(
          SchemaBuilder.with( schema)
          .maxItems( 0)
          .items( null)
          .build())
        .build(),

        VarValueDefBuilder.with( "anySize")
        .schema(
          SchemaBuilder.with( schema)
          .minItems( 1)
          .build())
        .build(),

        VarValueDefBuilder.with( "wrongItems")
        .type( FAILURE)
        .schema(
          SchemaBuilder.with( schema)
          .items( Schemas.not( schema.getItems()))
          .build())
        .build()
      };

    assertThat( "Values", values, containsMembers( VarValueDefMatcher::new, expected));
    }

  @Test
  public void whenMaxItemsNegative()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "array")
      .minItems( 2)
      .maxItems( -1)
      .uniqueItems( false)
      .build();
        
    // When...
    List<VarValueDef> values = resolver.valuesForSchema( schema).collect( toList());
    
    // Then...
    VarValueDef[] expected =
      new VarValueDef[]
      {
        VarValueDefBuilder.with( "minimumSize")
        .schema(
          SchemaBuilder.with( schema)
          .maxItems( 2)
          .build())
        .build(),

        VarValueDefBuilder.with( "tooSmall")
        .type( FAILURE)
        .schema(
          SchemaBuilder.with( schema)
          .minItems( 0)
          .maxItems( 1)
          .build())
        .build(),

        VarValueDefBuilder.with( "anySize")
        .schema(
          SchemaBuilder.with( schema)
          .minItems( 3)
          .maxItems( null)
          .build())
        .build()
      };

    assertThat( "Values", values, containsMembers( VarValueDefMatcher::new, expected));
    }

  @Test
  public void whenMinItemsNegative()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "array")
      .format( "list")
      .minItems( -1)
      .maxItems( 10)
      .items(
        SchemaBuilder.type( "integer")
        .format( "int32")
        .minimum( -1)
        .maximum( 1)
        .build())
      .build();
        
    // When...
    List<VarValueDef> values = resolver.valuesForSchema( schema).collect( toList());
    
    // Then...
    VarValueDef[] expected =
      new VarValueDef[]
      {
        VarValueDefBuilder.with( "empty")
        .schema(
          SchemaBuilder.with( schema)
          .maxItems( 0)
          .items( null)
          .build())
        .build(),

        VarValueDefBuilder.with( "maximumSize")
        .schema(
          SchemaBuilder.with( schema)
          .minItems( 10)
          .build())
        .build(),

        VarValueDefBuilder.with( "tooLarge")
        .type( FAILURE)
        .schema(
          SchemaBuilder.with( schema)
          .minItems( 11)
          .maxItems( null)
          .build())
        .build(),

        VarValueDefBuilder.with( "wrongItems")
        .type( FAILURE)
        .schema(
          SchemaBuilder.with( schema)
          .items( Schemas.not( schema.getItems()))
          .build())
        .build()
      };

    assertThat( "Values", values, containsMembers( VarValueDefMatcher::new, expected));
    }

  @Test
  public void whenArraySingleton()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "array")
      .minItems( 0)
      .maxItems( 1)
      .uniqueItems( true)
      .items(
        SchemaBuilder.type( "number")
        .multipleOf( "0.5")
        .build())
      .build();
        
    // When...
    List<VarValueDef> values = resolver.valuesForSchema( schema).collect( toList());
    
    // Then...
    VarValueDef[] expected =
      new VarValueDef[]
      {
        VarValueDefBuilder.with( "minimumSize")
        .schema(
          SchemaBuilder.with( schema)
          .maxItems( 0)
          .build())
        .build(),

        VarValueDefBuilder.with( "maximumSize")
        .schema(
          SchemaBuilder.with( schema)
          .minItems( 1)
          .build())
        .build(),

        VarValueDefBuilder.with( "tooLarge")
        .type( FAILURE)
        .schema(
          SchemaBuilder.with( schema)
          .minItems( 2)
          .maxItems( null)
          .build())
        .build(),

        VarValueDefBuilder.with( "wrongItems")
        .type( FAILURE)
        .schema(
          SchemaBuilder.with( schema)
          .items( Schemas.not( schema.getItems()))
          .build())
        .build()
      };

    assertThat( "Values", values, containsMembers( VarValueDefMatcher::new, expected));
    }

  @Test
  public void whenArrayConstant()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "array")
      .constant( arrayOf( -1, 0, 1))
      .build();
        
    // When...
    List<VarValueDef> values = resolver.valuesForSchema( schema).collect( toList());
    
    // Then...
    VarValueDef[] expected =
      new VarValueDef[]
      {
        new VarValueDef( Arrays.asList( -1, 0, 1))
      };

    assertThat( "Values", values, containsMembers( VarValueDefMatcher::new, expected));
    }
  
  @Test
  public void whenBoolean()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "boolean")
      .build();
        
    // When...
    List<VarValueDef> values = resolver.valuesForSchema( schema).collect( toList());
    
    // Then...
    VarValueDef[] expected =
      new VarValueDef[]
      {
        new VarValueDef( true),
        new VarValueDef( false)
      };

    assertThat( "Values", values, containsMembers( VarValueDefMatcher::new, expected));
    }
  @Test
  public void whenMinimumAboveMax()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "number")
      .minimum( 13)
      .maximum( "12.34")
      .exclusiveMinimum( "-12.34")
      .exclusiveMaximum( "12.34")
      .build();
        
    // When...
    List<VarValueDef> values = resolver.valuesForSchema( schema).collect( toList());
    
    // Then...
    VarValueDef[] expected =
      new VarValueDef[]
      {
        VarValueDefBuilder.with( "minimum")
        .schema(
          SchemaBuilder.with( schema)
          .minimum( "12.33")
          .build())
        .build(),

        VarValueDefBuilder.with( "belowMinimum")
        .type( FAILURE)
        .schema(
          SchemaBuilder.with( schema)
          .exclusiveMaximum( "12.33")
          .minimum( bigDecimalNull())
          .maximum( bigDecimalNull())
          .build())
        .build(),

        VarValueDefBuilder.with( "aboveMaximum")
        .type( FAILURE)
        .schema(
          SchemaBuilder.with( schema)
          .exclusiveMinimum( "12.33")
          .minimum( bigDecimalNull())
          .maximum( bigDecimalNull())
          .build())
        .build()
      };

    assertThat( "Values", values, containsMembers( VarValueDefMatcher::new, expected));
    }

  @Test
  public void whenMaximumBelowExclusiveMax()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "number")
      .format( "double")
      .minimum( "-10.6")
      .maximum( "9.9")
      .exclusiveMinimum( "-10.5")
      .exclusiveMaximum( "10.5")
      .multipleOf( "0.5")
      .build();
        
    // When...
    List<VarValueDef> values = resolver.valuesForSchema( schema).collect( toList());
    
    // Then...
    VarValueDef[] expected =
      new VarValueDef[]
      {
        VarValueDefBuilder.with( "minimum")
        .schema(
          SchemaBuilder.with( schema)
          .minimum( "-10.0")
          .maximum( "-10.0")
          .build())
        .build(),

        VarValueDefBuilder.with( "maximum")
        .schema(
          SchemaBuilder.with( schema)
          .minimum( "9.5")
          .maximum( "9.5")
          .build())
        .build(),

        VarValueDefBuilder.with( "belowMinimum")
        .type( FAILURE)
        .schema(
          SchemaBuilder.with( schema)
          .exclusiveMaximum( "-10.0")
          .minimum( bigDecimalNull())
          .maximum( bigDecimalNull())
          .build())
        .build(),

        VarValueDefBuilder.with( "aboveMaximum")
        .type( FAILURE)
        .schema(
          SchemaBuilder.with( schema)
          .exclusiveMinimum( "9.5")
          .minimum( bigDecimalNull())
          .maximum( bigDecimalNull())
          .build())
        .build(),

        VarValueDefBuilder.with( "notMultiple")
        .type( FAILURE)
        .schema(
          SchemaBuilder.type( "number")
          .constant( bigDecimalOf( "9.4"))
          .build())
        .build()
      };

    assertThat( "Values", values, containsMembers( VarValueDefMatcher::new, expected));
    }

  @Test
  public void whenMaximumAboveExclusiveMax()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "number")
      .maximum( "10.5")
      .exclusiveMaximum( "10.5")
      .build();
        
    // When...
    List<VarValueDef> values = resolver.valuesForSchema( schema).collect( toList());
    
    // Then...
    VarValueDef[] expected =
      new VarValueDef[]
      {
        VarValueDefBuilder.with( "belowMaximum")
        .schema(
          SchemaBuilder.with( schema)
          .exclusiveMaximum( "10.4")
          .maximum( bigDecimalNull())
          .build())
        .build(),

        VarValueDefBuilder.with( "maximum")
        .schema(
          SchemaBuilder.with( schema)
          .minimum( "10.4")
          .maximum( "10.4")
          .build())
        .build(),

        VarValueDefBuilder.with( "aboveMaximum")
        .type( FAILURE)
        .schema(
          SchemaBuilder.with( schema)
          .exclusiveMinimum( "10.4")
          .maximum( bigDecimalNull())
          .build())
        .build()
      };

    assertThat( "Values", values, containsMembers( VarValueDefMatcher::new, expected));
    }

  @Test
  public void whenMinimumAboveExclusiveMin()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "number")
      .format( "float")
      .minimum( "10.1")
      .maximum( "10.0")
      .exclusiveMinimum( "-10.5")
      .multipleOf( "0.5")
      .build();
        
    // When...
    List<VarValueDef> values = resolver.valuesForSchema( schema).collect( toList());
    
    // Then...
    VarValueDef[] expected =
      new VarValueDef[]
      {
        VarValueDefBuilder.with( "minimum")
        .schema(
          SchemaBuilder.with( schema)
          .minimum( "10.0")
          .maximum( "10.0")
          .build())
        .build(),

        VarValueDefBuilder.with( "belowMinimum")
        .type( FAILURE)
        .schema(
          SchemaBuilder.with( schema)
          .exclusiveMaximum( "10.0")
          .minimum( bigDecimalNull())
          .maximum( bigDecimalNull())
          .build())
        .build(),

        VarValueDefBuilder.with( "aboveMaximum")
        .type( FAILURE)
        .schema(
          SchemaBuilder.with( schema)
          .exclusiveMinimum( "10.0")
          .minimum( bigDecimalNull())
          .maximum( bigDecimalNull())
          .build())
        .build()
      };

    assertThat( "Values", values, containsMembers( VarValueDefMatcher::new, expected));
    }

  @Test
  public void whenMultipleOfUnit()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "number")
      .minimum( "-1.2")
      .multipleOf( "0.1")
      .build();
        
    // When...
    List<VarValueDef> values = resolver.valuesForSchema( schema).collect( toList());
    
    // Then...
    VarValueDef[] expected =
      new VarValueDef[]
      {
        VarValueDefBuilder.with( "minimum")
        .schema(
          SchemaBuilder.with( schema)
          .minimum( "-1.2")
          .maximum( "-1.2")
          .build())
        .build(),

        VarValueDefBuilder.with( "aboveMinimum")
        .schema(
          SchemaBuilder.with( schema)
          .exclusiveMinimum( "-1.2")
          .minimum( bigDecimalNull())
          .build())
        .build(),

        VarValueDefBuilder.with( "belowMinimum")
        .type( FAILURE)
        .schema(
          SchemaBuilder.with( schema)
          .exclusiveMaximum( "-1.2")
          .minimum( bigDecimalNull())
          .maximum( bigDecimalNull())
          .build())
        .build(),

        VarValueDefBuilder.with( "notMultiple")
        .type( FAILURE)
        .schema(
          SchemaBuilder.type( "number")
          .constant( bigDecimalOf( "-1.15"))
          .build())
        .build()
      };

    assertThat( "Values", values, containsMembers( VarValueDefMatcher::new, expected));
    }

  @Test
  public void whenAnyMultiple()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "integer")
      .format( "int32")
      .multipleOf( -11)
      .build();
        
    // When...
    List<VarValueDef> values = resolver.valuesForSchema( schema).collect( toList());
    
    // Then...
    VarValueDef[] expected =
      new VarValueDef[]
      {
        VarValueDefBuilder.with( "negative")
        .schema(
          SchemaBuilder.with( schema)
          .exclusiveMaximum( 0)
          .build())
        .build(),

        VarValueDefBuilder.with( "zero")
        .schema(
          SchemaBuilder.type( "number")
          .constant( bigDecimalOf( "0"))
          .build())
        .build(),

        VarValueDefBuilder.with( "positive")
        .schema(
          SchemaBuilder.with( schema)
          .exclusiveMinimum( 0)
          .build())
        .build(),

        VarValueDefBuilder.with( "notMultiple")
        .type( FAILURE)
        .schema(
          SchemaBuilder.type( "number")
          .constant( bigDecimalOf( "1"))
          .build())
        .build()
      };

    assertThat( "Values", values, containsMembers( VarValueDefMatcher::new, expected));
    }

  @Test
  public void whenStringNormal()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "string")
      .minLength( 4)
      .maxLength( 8)
      .build();
        
    // When...
    List<VarValueDef> values = resolver.valuesForSchema( schema).collect( toList());
    
    // Then...
    VarValueDef[] expected =
      new VarValueDef[]
      {
        VarValueDefBuilder.with( "minimumLength")
        .schema(
          SchemaBuilder.with( schema)
          .minLength( 4)
          .maxLength( 4)
          .build())
        .build(),

        VarValueDefBuilder.with( "maximumLength")
        .schema(
          SchemaBuilder.with( schema)
          .minLength( 8)
          .maxLength( 8)
          .build())
        .build(),

        VarValueDefBuilder.with( "tooShort")
        .type( FAILURE)
        .schema(
          SchemaBuilder.with( schema)
          .minLength( null)
          .maxLength( 3)
          .build())
        .build(),

        VarValueDefBuilder.with( "tooLong")
        .type( FAILURE)
        .schema(
          SchemaBuilder.with( schema)
          .minLength( 9)
          .maxLength( null)
          .build())
        .build()
      };

    assertThat( "Values", values, containsMembers( VarValueDefMatcher::new, expected));
    }

  @Test
  public void whenMaxLengthAbovePatternMax()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "string")
      .format( "address")
      .minLength( -1)
      .maxLength( 16)
      .pattern( "^X[0-9]@my.org$")
      .build();
        
    // When...
    List<VarValueDef> values = resolver.valuesForSchema( schema).collect( toList());
    
    // Then...
    VarValueDef[] expected =
      new VarValueDef[]
      {
        VarValueDefBuilder.with( "minimumLength")
        .schema(
          SchemaBuilder.with( schema)
          .minLength( 9)
          .build())
        .build(),

        VarValueDefBuilder.with( "wrongPattern")
        .type( FAILURE)
        .schema(
          SchemaBuilder.with( schema)
          .minLength( 9)
          .maxLength( 9)
          .pattern( "^([ !\"#$%&\\'()*+,\\-./0123456789:;<=>?]{1,}|[X]{0,8})$")
          .build())
        .build()
      };

    assertThat( "Values", values, containsMembers( VarValueDefMatcher::new, expected));
    }

  @Test
  public void whenMinLengthBelowFormatMin()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "string")
      .format( "email")
      .minLength( 4)
      .maxLength( 32)
      .build();
        
    // When...
    List<VarValueDef> values = resolver.valuesForSchema( schema).collect( toList());
    
    // Then...
    VarValueDef[] expected =
      new VarValueDef[]
      {
        VarValueDefBuilder.with( "minimumLength")
        .schema(
          SchemaBuilder.with( schema)
          .minLength( 7)
          .maxLength( 7)
          .build())
        .build(),

        VarValueDefBuilder.with( "maximumLength")
        .schema(
          SchemaBuilder.with( schema)
          .minLength( 32)
          .maxLength( 32)
          .build())
        .build(),

        VarValueDefBuilder.with( "tooLong")
        .type( FAILURE)
        .schema(
          SchemaBuilder.with( schema)
          .minLength( 33)
          .maxLength( null)
          .build())
        .build(),

        VarValueDefBuilder.with( "wrongFormat")
        .type( FAILURE)
        .schema(
          SchemaBuilder.type( "string")
          .format( "date-time")
          .build())
        .build()
      };

    assertThat( "Values", values, containsMembers( VarValueDefMatcher::new, expected));
    }

  @Test
  public void whenMaxLengthAboveFormatMax()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "string")
      .format( "date-time")
      .minLength( 0)
      .maxLength( 100)
      .pattern( "2022")
      .build();
        
    // When...
    List<VarValueDef> values = resolver.valuesForSchema( schema).collect( toList());
    
    // Then...
    VarValueDef[] expected =
      new VarValueDef[]
      {
        VarValueDefBuilder.with( "minimumLength")
        .schema(
          SchemaBuilder.with( schema)
          .maxLength( 29)
          .build())
        .build(),

        VarValueDefBuilder.with( "wrongFormat")
        .type( FAILURE)
        .schema(
          SchemaBuilder.type( "string")
          .format( "email")
          .build())
        .build()
      };

    assertThat( "Values", values, containsMembers( VarValueDefMatcher::new, expected));
    }

  @Test
  public void whenMinLengthBelowPatternMin()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "string")
      .minLength( 4)
      .maxLength( -1)
      .pattern( "^X[0-9]?@my.org$")
      .build();
        
    // When...
    List<VarValueDef> values = resolver.valuesForSchema( schema).collect( toList());
    
    // Then...
    VarValueDef[] expected =
      new VarValueDef[]
      {
        VarValueDefBuilder.with( "minimumLength")
        .schema(
          SchemaBuilder.with( schema)
          .minLength( 8)
          .maxLength( 8)
          .build())
        .build(),

        VarValueDefBuilder.with( "maximumLength")
        .schema(
          SchemaBuilder.with( schema)
          .minLength( 9)
          .maxLength( 9)
          .build())
        .build(),

        VarValueDefBuilder.with( "wrongPattern")
        .type( FAILURE)
        .schema(
          SchemaBuilder.with( schema)
          .minLength( 8)
          .maxLength( 9)
          .pattern( "^([ !\"#$%&\\'()*+,\\-./0123456789:;<=>?]{1,}|[X]{0,7})$")
          .build())
        .build()
      };

    assertThat( "Values", values, containsMembers( VarValueDefMatcher::new, expected));
    }

  @Test
  public void whenAnyString()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "string")
      .format( "label")
      .build();
        
    // When...
    List<VarValueDef> values = resolver.valuesForSchema( schema).collect( toList());
    
    // Then...
    VarValueDef[] expected =
      new VarValueDef[]
      {
        VarValueDefBuilder.with( "empty")
        .schema(
          SchemaBuilder.with( schema)
          .maxLength( 0)
          .build())
        .build(),

        VarValueDefBuilder.with( "anyLength")
        .schema(
          SchemaBuilder.with( schema)
          .minLength( 1)
          .maxLength( null)
          .build())
        .build()
      };

    assertThat( "Values", values, containsMembers( VarValueDefMatcher::new, expected));
    }

  @Test
  public void whenPattern()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "string")
      .minLength( 10)
      .maxLength( 20)
      .pattern( "^X{1,100}$")
      .build();
        
    // When...
    List<VarValueDef> values = resolver.valuesForSchema( schema).collect( toList());
    
    // Then...
    VarValueDef[] expected =
      new VarValueDef[]
      {
        VarValueDefBuilder.with( "minimumLength")
        .schema(
          SchemaBuilder.with( schema)
          .maxLength( 10)
          .build())
        .build(),

        VarValueDefBuilder.with( "maximumLength")
        .schema(
          SchemaBuilder.with( schema)
          .minLength( 20)
          .build())
        .build(),

        VarValueDefBuilder.with( "tooShort")
        .type( FAILURE)
        .schema(
          SchemaBuilder.with( schema)
          .minLength( null)
          .maxLength( 9)
          .build())
        .build(),

        VarValueDefBuilder.with( "tooLong")
        .type( FAILURE)
        .schema(
          SchemaBuilder.with( schema)
          .minLength( 21)
          .maxLength( null)
          .build())
        .build(),

        VarValueDefBuilder.with( "wrongPattern")
        .type( FAILURE)
        .schema(
          SchemaBuilder.with( schema)
          .minLength( 10)
          .maxLength( 20)
          .pattern( "^([ !\"#$%&\\'()*+,\\-./0123456789:;<=>?]{1,}|[X]{0,0})$")
          .build())
        .build()
      };

    assertThat( "Values", values, containsMembers( VarValueDefMatcher::new, expected));
    }

  @Test
  public void whenMaxLength()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "string")
      .minLength( 0)
      .maxLength( 10)
      .build();
        
    // When...
    List<VarValueDef> values = resolver.valuesForSchema( schema).collect( toList());
    
    // Then...
    VarValueDef[] expected =
      new VarValueDef[]
      {
        VarValueDefBuilder.with( "minimumLength")
        .schema(
          SchemaBuilder.with( schema)
          .maxLength( 0)
          .build())
        .build(),

        VarValueDefBuilder.with( "maximumLength")
        .schema(
          SchemaBuilder.with( schema)
          .minLength( 10)
          .build())
        .build(),

        VarValueDefBuilder.with( "tooLong")
        .type( FAILURE)
        .schema(
          SchemaBuilder.with( schema)
          .minLength( 11)
          .maxLength( null)
          .build())
        .build()
      };

    assertThat( "Values", values, containsMembers( VarValueDefMatcher::new, expected));
    }

  @Test
  public void whenPatternNotApplicable()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "string")
      .format( "uuid")
      .pattern( "^[0-9]$")
      .build();
        
    // When...
    List<VarValueDef> values = resolver.valuesForSchema( schema).collect( toList());
    
    // Then...
    VarValueDef[] expected =
      new VarValueDef[]
      {
        VarValueDefBuilder.with( "minimumLength")
        .schema(
          SchemaBuilder.with( schema)
          .pattern( null)
          .build())
        .build(),

        VarValueDefBuilder.with( "wrongFormat")
        .type( FAILURE)
        .schema(
          SchemaBuilder.type( "string")
          .format( "email")
          .build())
        .build()
      };

    assertThat( "Values", values, containsMembers( VarValueDefMatcher::new, expected));
    assertWarnings( "Pattern matching not supported for strings with format=uuid. Ignoring the pattern for this schema.");
    }

  @Test
  public void whenConstantString()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "string")
      .format( "email")
      .constant( "me@my.org")
      .build();
        
    // When...
    List<VarValueDef> values = resolver.valuesForSchema( schema).collect( toList());
    
    // Then...
    VarValueDef[] expected =
      new VarValueDef[]
      {
        new VarValueDef( "me@my.org")
      };

    assertThat( "Values", values, containsMembers( VarValueDefMatcher::new, expected));
    }

  @Test
  public void whenConstantNull()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "string")
      .constant( nullValue())
      .build();
        
    // When...
    List<VarValueDef> values = resolver.valuesForSchema( schema).collect( toList());
    
    // Then...
    VarValueDef[] expected =
      new VarValueDef[]
      {
        new VarValueDef( (Object) null)
      };

    assertThat( "Values", values, containsMembers( VarValueDefMatcher::new, expected));
    }
  }
