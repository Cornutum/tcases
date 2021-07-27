//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2021, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import java.util.Arrays;
import static java.util.Collections.emptyList;

import org.junit.Test;

/**
 * Runs tests for request parameter normalization
 */
public class NormalizeTest extends OpenApiTest
  {
  @Test
  public void whenStyleMatrix()
    {
    // When...
    verifyRequestInputModel( "normalize-matrix");
    
    // Then...
    assertConditions(
      emptyList(),

      Arrays.asList(
        "Normalize,/path/string/{empty}/{nonEmpty}/{nullable},GET,nullable: Null values not allowed. Using nullable=false.",
        "Normalize,/path/array/{empty}/{nonEmpty}/{nullable}/{exploded},GET,nullable: Null values not allowed. Using nullable=false.",
        "Normalize,/path/object/{nonEmpty}/{nullable}/{exploded},GET,nullable: Null values not allowed. Using nullable=false."));
    }
  
  @Test
  public void whenStyleLabel()
    {
    // When...
    verifyRequestInputModel( "normalize-label");
    
    // Then...
    assertConditions(
      emptyList(),

      Arrays.asList(
        "Normalize,/path/string/{empty}/{nonEmpty}/{nullable},GET,nullable: Null values not allowed. Using nullable=false.",
        "Normalize,/path/array/{empty}/{nonEmpty}/{nullable}/{exploded},GET,nullable: Null values not allowed. Using nullable=false.",
        "Normalize,/path/object/{nonEmpty}/{nullable}/{exploded},GET,nullable: Null values not allowed. Using nullable=false."));
    }
  
  @Test
  public void whenStyleSimple()
    {
    // When...
    verifyRequestInputModel( "normalize-simple");
    
    // Then...
    assertConditions(
      Arrays.asList(
        "Normalize,/path/string/{empty}/{nonEmpty}/{nullable},GET,empty: Empty string values not allowed for non-nullable parameter -- using minLength=1.",
        "Normalize,/path/string/{empty}/{nonEmpty}/{nullable},GET,nullable: Empty string values not allowed for non-nullable parameter -- using minLength=1.",
        "Normalize,/path/array/{empty}/{nonEmpty}/{nullable}/{exploded},GET,empty: Empty array values not allowed for non-nullable parameter -- using minItems=1.",
        "Normalize,/path/array/{empty}/{nonEmpty}/{nullable}/{exploded},GET,nullable: Empty array values not allowed for non-nullable parameter -- using minItems=1.",
        "Normalize,/path/array/{empty}/{nonEmpty}/{nullable}/{exploded},GET,exploded: Empty array values not allowed for non-nullable parameter -- using minItems=1.",
        "Normalize,/header/string,GET,empty: Empty string values not allowed for non-nullable parameter -- using minLength=1.",
        "Normalize,/header/string,GET,nullableNonEmpty: Empty string values allowed for nullable parameter -- using minLength=0.",
        "Normalize,/header/array,GET,empty: Empty array values not allowed for non-nullable parameter -- using minItems=1.",
        "Normalize,/header/array,GET,nullableNonEmpty: Empty array values allowed for nullable parameter -- using minItems=0.",
        "Normalize,/header/array,GET,exploded: Empty array values not allowed for non-nullable parameter -- using minItems=1."),

      Arrays.asList(
        "Normalize,/path/string/{empty}/{nonEmpty}/{nullable},GET,nullable: Null values not allowed. Using nullable=false.",
        "Normalize,/path/array/{empty}/{nonEmpty}/{nullable}/{exploded},GET,nullable: Null values not allowed. Using nullable=false.",
        "Normalize,/path/object/{nonEmpty}/{nullable}/{exploded},GET,nullable: Null values not allowed. Using nullable=false."));
    }
  
  @Test
  public void whenStyleForm()
    {
    // When...
    verifyRequestInputModel( "normalize-form");
    
    // Then...
    assertConditions( Arrays.asList(), Arrays.asList());
    }

  /**
   * Returns the {@link ModelOptions} used for this test.
   */
  @Override
  protected ModelOptions getModelOptions()
    {
    return withConditionRecorder();
    }
  }
