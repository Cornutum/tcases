//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.*;
import org.cornutum.tcases.resolve.*;
import static org.cornutum.tcases.resolve.DataValues.*;

import org.junit.Test;

/**
 * Runs tests for input models that define "boolean" schemas.
 */
public class BooleanSchemaTest extends SystemInputJsonTest
  {
  @Test
  public void Schemas_0()
    {
    expectSystemInputJson(
      "schema-boolean-0.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "boolean")
        .vars(
          VarDefBuilder.with( "booleanVar")
          .schema(
            SchemaBuilder.type( "boolean")
            .build())
          .values(
            VarValueDefBuilder.with( "booleanValue")
            .schema(
              SchemaBuilder.type( "boolean")
              .constant( false)
              .build())
            .build(),
            VarValueDefBuilder.with( "undefined")
            .schema(
              SchemaBuilder.type( "boolean")
              .constant( nullValue())
              .build())
            .build())
          .build())
        .build())
      .build());
    }

  @Test
  public void Schemas_1()
    {
    assertDefinitionError( "schema-boolean-1.json", "Schema has type=boolean but defines a 'const' of type=string");
    }
  }
