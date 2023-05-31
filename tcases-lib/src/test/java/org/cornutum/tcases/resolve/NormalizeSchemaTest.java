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
 * Runs tests for {@link Schemas#normalize}
 */
public class NormalizeSchemaTest extends ResolverTest
  {
  @Test
  public void whenMinItemsAboveMax()
    {
    // Given...
    Schemas schemas = new Schemas( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "array")
      .minItems( 10)
      .maxItems( 4)
      .uniqueItems( true)
      .items( SchemaBuilder.type( "number").minimum( "0.0").maximum( "1.0").build())
      .build();

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = schemas.normalize( normalized);
    
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
    Schemas schemas = new Schemas( withConditionRecorder());

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
    normalized = schemas.normalize( normalized);
    
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
    Schemas schemas = new Schemas( withConditionRecorder());

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
    normalized = schemas.normalize( normalized);
    
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
    Schemas schemas = new Schemas( withConditionRecorder());

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
    normalized = schemas.normalize( normalized);
    
    // Then...
    Schema expected =
      SchemaBuilder.with( schema)
      .minItems( null)
      .build();

    assertThat( "Normalized", normalized, matches( new SchemaMatcher( expected)));
    assertConditionsNone();
    }

  @Test
  public void whenMaxItemsNoneInfeasible()
    {
    // Given...
    Schemas schemas = new Schemas( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "array")
      .uniqueItems( true)
      .items( SchemaBuilder.type( "string").enums( "A", "B", "C").build())
      .build();

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = schemas.normalize( normalized);
    
    // Then...
    Schema expected =
      SchemaBuilder.with( schema)
      .maxItems( 3)
      .build();

    assertThat( "Normalized", normalized, matches( new SchemaMatcher( expected)));
    assertErrors( "maxItems exceeds the number of unique item values. Adjusting maxItems to max unique items=3.");
    }

  @Test
  public void whenMaxItemsInfeasible()
    {
    // Given...
    Schemas schemas = new Schemas( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "array")
      .uniqueItems( true)
      .maxItems( 4)
      .items( SchemaBuilder.type( "string").enums( "A", "B", "C").build())
      .build();

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = schemas.normalize( normalized);
    
    // Then...
    Schema expected =
      SchemaBuilder.with( schema)
      .maxItems( 3)
      .build();

    assertThat( "Normalized", normalized, matches( new SchemaMatcher( expected)));
    assertErrors( "maxItems exceeds the number of unique item values. Adjusting maxItems to max unique items=3.");
    }

  @Test
  public void whenClassifierConstantEnum()
    {
    // Given...
    Schemas schemas = new Schemas( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "array")
      .enums( arrayOfAny( valueOf( bigDecimalOf( "1.23")), valueOf( 4), valueOf( 5)))
      .minItems( 1)
      .maxItems( 4)
      .uniqueItems( true)
      .items( SchemaBuilder.type( "integer").build())
      .build();

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = schemas.normalize( normalized);
    
    // Then...
    Schema expected =
      SchemaBuilder.type( "array")
      .constant( arrayOfAny( valueOf( bigDecimalOf( "1.23")), valueOf( 4), valueOf( 5)))
      .build();

    assertThat( "Normalized", normalized, matches( new SchemaMatcher( expected)));
    assertWarnings( "Values defined using 'const'. Ignoring all other schema properties.");
    }

  @Test
  public void whenMinimumAboveMax()
    {
    // Given...
    Schemas schemas = new Schemas( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "number")
      .minimum( 13)
      .maximum( "12.34")
      .exclusiveMinimum( "-12.34")
      .exclusiveMaximum( "12.34")
      .multipleOf( 0)
      .build();

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = schemas.normalize( normalized);
    
    // Then...
    Schema expected =
      SchemaBuilder.with( schema)
      .minimum( "12.33")
      .maximum( "12.33")
      .exclusiveMinimum( bigDecimalNull())
      .exclusiveMaximum( bigDecimalNull())
      .multipleOf( bigDecimalNull())
      .build();

    assertThat( "Normalized", normalized, matches( new SchemaMatcher( expected)));
    assertErrors( "minimum=13 is greater than maximum=12.33. Adjusting minimum to maximum.");
    }

  @Test
  public void whenMaximumBelowExclusiveMax()
    {
    // Given...
    Schemas schemas = new Schemas( withConditionRecorder());

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
    normalized = schemas.normalize( normalized);
    
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
    Schemas schemas = new Schemas( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "number")
      .maximum( "10.5")
      .exclusiveMaximum( "10.5")
      .build();

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = schemas.normalize( normalized);
    
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
    Schemas schemas = new Schemas( withConditionRecorder());

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
    normalized = schemas.normalize( normalized);
    
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
    Schemas schemas = new Schemas( withConditionRecorder());

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
    normalized = schemas.normalize( normalized);
    
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
    Schemas schemas = new Schemas( withConditionRecorder());

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
    normalized = schemas.normalize( normalized);
    
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
  public void whenClassifierConst()
    {
    // Given...
    Schemas schemas = new Schemas( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "number")
      .format( "double")
      .constant( bigDecimalOf( "2.5"))
      .enums( bigDecimalOf( "-0.5"), bigDecimalOf( "0.5"))
      .minimum( "-1.6")
      .maximum( "2.5")
      .multipleOf( "0.5")
      .build();

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = schemas.normalize( normalized);
    
    // Then...
    Schema expected =
      SchemaBuilder.type( "number")
      .format( "double")
      .constant( bigDecimalOf( "2.5"))
      .build();

    assertThat( "Normalized", normalized, matches( new SchemaMatcher( expected)));
    assertWarnings( "Values defined using 'const'. Ignoring all other schema properties.");
    }

  @Test
  public void whenStringNormal()
    {
    // Given...
    Schemas schemas = new Schemas( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "string")
      .minLength( 4)
      .maxLength( 8)
      .build();

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = schemas.normalize( normalized);
    
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
    Schemas schemas = new Schemas( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "string")
      .format( "contact")
      .minLength( -1)
      .maxLength( 16)
      .pattern( "^X[0-9]@my.org$")
      .build();

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = schemas.normalize( normalized);
    
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
    Schemas schemas = new Schemas( withConditionRecorder());

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
    normalized = schemas.normalize( normalized);
    
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
    Schemas schemas = new Schemas( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "string")
      .format( "date-time")
      .minLength( 0)
      .maxLength( 100)
      .pattern( null)
      .build();

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = schemas.normalize( normalized);
    
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
    Schemas schemas = new Schemas( withConditionRecorder());

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
    normalized = schemas.normalize( normalized);
    
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
  public void whenPatternNotApplicable()
    {
    // Given...
    Schemas schemas = new Schemas( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "string")
      .format( "uuid")
      .pattern( "\\D+")
      .build();

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = schemas.normalize( normalized);
    
    // Then...
    Schema expected =
      SchemaBuilder.with( schema)
      .minLength( 36)
      .maxLength( 36)
      .pattern( null)
      .build();

    assertThat( "Normalized", normalized, matches( new SchemaMatcher( expected)));
    assertWarnings( "Pattern matching not supported for strings with format=uuid. Ignoring the pattern for this schema.");
    }

  @Test
  public void whenPatternInvalid()
    {
    // Given...
    Schemas schemas = new Schemas( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "string")
      .pattern( "[/w{2,3}")
      .build();

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = schemas.normalize( normalized);
    
    // Then...
    Schema expected =
      SchemaBuilder.with( schema)
      .pattern( null)
      .build();

    assertThat( "Normalized", normalized, matches( new SchemaMatcher( expected)));
    assertErrors( "Invalid pattern: Missing ']' at position=8. Ignoring the pattern for this schema.");
    }

  @Test
  public void whenClassifierEnum()
    {
    // Given...
    Schemas schemas = new Schemas( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "string")
      .format( "date")
      .enums( "2000-01-03", null, "2007-08-09")
      .minLength( 1)
      .maxLength( 2)
      .pattern( ".*")
      .build();

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = schemas.normalize( normalized);
    
    // Then...
    Schema expected =
      SchemaBuilder.type( "string")
      .format( "date")
      .enums( dateOf("2000-01-03"), noValue(), dateOf("2007-08-09"))
      .build();

    assertThat( "Normalized", normalized, matches( new SchemaMatcher( expected)));
    assertWarnings( "Values defined using 'enum'. Ignoring all other schema properties.");
    }

  @Test
  public void whenEnumFormatWrong()
    {
    // Given...
    Schemas schemas = new Schemas( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "string")
      .format( "email")
      .enums( "me@myself.org", "2004-05-06")
      .build();

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    assertResolverException(
      () -> schemas.normalize( normalized),
      "Error processing enum",
      "Invalid email value=2004-05-06");
    }

  @Test
  public void whenIntegerEnum()
    {
    // Given...
    Schemas schemas = new Schemas( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "integer")
      .enums( 1, 2, 3)
      .build();

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = schemas.normalize( normalized);
    
    // Then...
    Schema expected =
      SchemaBuilder.type( "integer")
      .enums( 1L, 2L, 3L)
      .build();

    assertThat( "Normalized", normalized, matches( new SchemaMatcher( expected)));
    assertConditionsNone();
    }

  @Test
  public void whenUnchanged()
    {
    // Given...
    Schemas schemas = new Schemas( withConditionRecorder());

    Schema schema =
      SchemaBuilder.type( "string")
      .format( "label")
      .build();

    Schema normalized =
      SchemaBuilder.with( schema)
      .build();
    
    // When...
    normalized = schemas.normalize( normalized);
    
    // Then...
    Schema expected = schema;
    assertThat( "Normalized", normalized, matches( new SchemaMatcher( expected)));
    assertConditionsNone();
    }
  }
