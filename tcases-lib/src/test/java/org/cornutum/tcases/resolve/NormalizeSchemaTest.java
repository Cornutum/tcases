//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import static org.cornutum.tcases.resolve.DataValues.*;

import org.junit.Test;
import static org.cornutum.hamcrest.Composites.*;
import static org.hamcrest.MatcherAssert.*;

/**
 * Runs tests for {@link TestCaseSchemaResolver#normalize}
 */
public class NormalizeSchemaTest extends ResolverTest
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

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = resolver.normalize( normalized);
    
    // Then...
    Schema expected =
      SchemaBuilder.with( schema)
      .minItems( 4)
      .build();

    assertThat( "Normalized", normalized, matches( new SchemaMatcher( expected)));
    assertErrors( "minItems=10 is greater than maxItems=4. Adjusting minItems to maxItems.");
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

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
     // When...
    normalized = resolver.normalize( normalized);
    
    // Then...
    Schema expected =
      SchemaBuilder.with( schema)
      .items(
        SchemaBuilder.with( schema.getItems())
        .minimum( 9)
        .maximum( 9)
        .exclusiveMinimum( bigDecimalNull())
        .exclusiveMaximum( bigDecimalNull())
        .build())
      .build();

    assertThat( "Normalized", normalized, matches( new SchemaMatcher( expected)));
    assertErrors( "items: minimum=12 is greater than maximum=9. Adjusting minimum to maximum.");
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

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = resolver.normalize( normalized);
    
    // Then...
    Schema expected =
      SchemaBuilder.with( schema)
      .maxItems( null)
      .build();

    assertThat( "Normalized", normalized, matches( new SchemaMatcher( expected)));
    assertConditionsNone();
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
        .build())
      .build();

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = resolver.normalize( normalized);
    
    // Then...
    Schema expected =
      SchemaBuilder.with( schema)
      .minItems( null)
      .build();

    assertThat( "Normalized", normalized, matches( new SchemaMatcher( expected)));
    assertConditionsNone();
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

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = resolver.normalize( normalized);
    
    // Then...
    Schema expected =
      SchemaBuilder.with( schema)
      .minimum( "12.33")
      .maximum( "12.33")
      .exclusiveMinimum( bigDecimalNull())
      .exclusiveMaximum( bigDecimalNull())
      .build();

    assertThat( "Normalized", normalized, matches( new SchemaMatcher( expected)));
    assertErrors( "minimum=13 is greater than maximum=12.33. Adjusting minimum to maximum.");
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

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = resolver.normalize( normalized);
    
    // Then...
    Schema expected =
      SchemaBuilder.with( schema)
      .minimum( "-10.0")
      .maximum( "9.5")
      .exclusiveMinimum( bigDecimalNull())
      .exclusiveMaximum( bigDecimalNull())
      .build();

    assertThat( "Normalized", normalized, matches( new SchemaMatcher( expected)));
    assertConditionsNone();
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

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = resolver.normalize( normalized);
    
    // Then...
    Schema expected =
      SchemaBuilder.with( schema)
      .maximum( "10.4")
      .exclusiveMinimum( bigDecimalNull())
      .exclusiveMaximum( bigDecimalNull())
      .build();

    assertThat( "Normalized", normalized, matches( new SchemaMatcher( expected)));
    assertConditionsNone();
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

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = resolver.normalize( normalized);
    
    // Then...
    Schema expected =
      SchemaBuilder.with( schema)
      .minimum( "10.0")
      .maximum( "10.0")
      .exclusiveMinimum( bigDecimalNull())
      .exclusiveMaximum( bigDecimalNull())
      .build();

    assertThat( "Normalized", normalized, matches( new SchemaMatcher( expected)));
    assertErrors( "minimum=10.5 is greater than maximum=10.0. Adjusting minimum to maximum.");
    }

  @Test
  public void whenMaximumNotMultiple()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "number")
      .minimum( 0)
      .maximum( "1.2")
      .exclusiveMaximum( "10.5")
      .multipleOf( "0.5")
      .build();

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = resolver.normalize( normalized);
    
    // Then...
    Schema expected =
      SchemaBuilder.with( schema)
      .maximum( "1.0")
      .exclusiveMinimum( bigDecimalNull())
      .exclusiveMaximum( bigDecimalNull())
      .build();

    assertThat( "Normalized", normalized, matches( new SchemaMatcher( expected)));
    assertConditionsNone();
    }

  @Test
  public void whenMinimumNotMultiple()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "number")
      .format( "double")
      .minimum( "-1.6")
      .exclusiveMinimum( "-10.5")
      .multipleOf( "0.5")
      .build();

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = resolver.normalize( normalized);
    
    // Then...
    Schema expected =
      SchemaBuilder.with( schema)
      .minimum( "-1.5")
      .exclusiveMinimum( bigDecimalNull())
      .exclusiveMaximum( bigDecimalNull())
      .build();

    assertThat( "Normalized", normalized, matches( new SchemaMatcher( expected)));
    assertConditionsNone();
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

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = resolver.normalize( normalized);
    
    // Then...
    Schema expected =
      SchemaBuilder.with( schema)
      .build();

    assertThat( "Normalized", normalized, matches( new SchemaMatcher( expected)));
    assertConditionsNone();
    }

  @Test
  public void whenMaxLengthAbovePatternMax()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "string")
      .format( "email")
      .minLength( -1)
      .maxLength( 16)
      .pattern( "^X[0-9]@my.org$")
      .build();

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = resolver.normalize( normalized);
    
    // Then...
    Schema expected =
      SchemaBuilder.with( schema)
      .minLength( 9)
      .maxLength( 9)
      .build();

    assertThat( "Normalized", normalized, matches( new SchemaMatcher( expected)));
    assertErrors( "maxLength=16 is greater than the required maximum=9. Adjusting maxLength to the required maximum.");
    }

  @Test
  public void whenMinLengthBelowFormatMin()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "string")
      .format( "date")
      .minLength( 9)
      .maxLength( 8)
      .build();

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = resolver.normalize( normalized);
    
    // Then...
    Schema expected =
      SchemaBuilder.with( schema)
      .minLength( 10)
      .maxLength( 10)
      .build();

    assertThat( "Normalized", normalized, matches( new SchemaMatcher( expected)));
    assertErrors(
      "maxLength=8 is less than the required minimum=10. Adjusting maxLength to the required minimum.",
      "minLength=9 is less than the required minimum=10. Adjusting minLength to the required minimum."  );
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

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = resolver.normalize( normalized);
    
    // Then...
    Schema expected =
      SchemaBuilder.with( schema)
      .minLength( 29)
      .maxLength( 29)
      .build();

    assertThat( "Normalized", normalized, matches( new SchemaMatcher( expected)));
    assertErrors(
      "maxLength=100 is greater than the required maximum=29. Adjusting maxLength to the required maximum.",
      "minLength=0 is less than the required minimum=29. Adjusting minLength to the required minimum.");
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

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = resolver.normalize( normalized);
    
    // Then...
    Schema expected =
      SchemaBuilder.with( schema)
      .minLength( 8)
      .maxLength( 9)
      .build();

    assertThat( "Normalized", normalized, matches( new SchemaMatcher( expected)));
    assertErrors( "minLength=4 is less than the required minimum=8. Adjusting minLength to the required minimum.");
    }

  @Test
  public void whenUnchanged()
    {
    // Given...
    TestCaseSchemaResolver resolver = new TestCaseSchemaResolver( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "string")
      .format( "label")
      .build();

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = resolver.normalize( normalized);
    
    // Then...
    Schema expected = schema;
    assertThat( "Normalized", normalized, matches( new SchemaMatcher( expected)));
    assertConditionsNone();
    }
  }
