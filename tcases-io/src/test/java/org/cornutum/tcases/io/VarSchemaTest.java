//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.*;
import org.cornutum.tcases.resolve.*;

import org.junit.Test;

/**
 * Runs tests for input models that define variable schemas.
 */
public class VarSchemaTest extends SystemInputJsonTest
  {
  @Test
  public void whenVarSchemaOnly()
    {
    expectSystemInputJson(
      "schema-var-only.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "string")
        .vars(
          VarDefBuilder.with( "stringVar")
          .schema(
            SchemaBuilder.type( "string")
            .format( "date")
            .minLength( 1)
            .pattern( ".")
            .build())
          .build())
        .build())
      .build());
    }

  @Test
  public void whenVarSchemaFailureOnly()
    {
    assertDefinitionError(
      "schema-var-failures.json",
      "Error processing Schema, string, stringVar",
      "No valid values defined for variable=stringVar");
    }

  @Test
  public void whenVarSchemaValuesNone()
    {
    assertDefinitionError(
      "schema-var-values-none.json",
      "Error processing Schema, string, stringVar",
      "No valid values defined for variable=stringVar");
    }
  }
