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
 * Runs tests for input models that define "array" schemas.
 */
public class ArraySchemaTest extends SystemInputJsonTest
  {
  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Schemas (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> array </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Defined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minItems </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maxItems </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.uniqueItems </TD> <TD> Boolean </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.items </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_0()
    {
    expectSystemInputJson(
      "schema-array-0.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "array")
        .vars(
          VarDefBuilder.with( "arrayVar")
          .values(
            VarValueDefBuilder.with( "arrayValue")
            .schema(
              SchemaBuilder.type( "array")
              .format( "flat")
              .maxItems( 8)
              .uniqueItems()
              .build())
            .build())
          .build())
        .build())
      .build());
    }

  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Schemas (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Array </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minItems </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maxItems </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.uniqueItems </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.items </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_1()
    {
    expectSystemInputJson(
      "schema-array-1.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "array")
        .vars(
          VarDefBuilder.with( "arrayVar")
          .values(
            VarValueDefBuilder.with( "arrayValue")
            .schema(
              SchemaBuilder.type( "array")
              .constant( arrayOf( -1, 0, 1))
              .build())
            .build())
          .build())
        .build())
      .build());
    }

  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Schemas (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> array </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Defined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minItems </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maxItems </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.uniqueItems </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.items </TD> <TD> Schema </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_2()
    {
    expectSystemInputJson(
      "schema-array-2.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "array")
        .vars(
          VarDefBuilder.with( "arrayVar")
          .values(
            VarValueDefBuilder.with( "arrayValue")
            .schema(
              SchemaBuilder.type( "array")
              .format( "list")
              .minItems( 3)
              .items(
                SchemaBuilder.type( "string")
                .format( "email")
                .build())
              .build())
            .build())
          .build())
        .build())
      .build());
    }

  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Schemas (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> null </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minItems </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maxItems </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.uniqueItems </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.items </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_3()
    {
    expectSystemInputJson(
      "schema-array-3.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "array")
        .vars(
          VarDefBuilder.with( "arrayVar")
          .values(
            VarValueDefBuilder.with( "arrayValue")
            .schema(
              SchemaBuilder.generic()
              .format( "list")
              .constant( nullValue())
              .build())
            .build())
          .build())
        .build())
      .build());
    }

  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Schemas (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> array </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minItems </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maxItems </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.uniqueItems </TD> <TD> Boolean </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.items </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_4()
    {
    assertDefinitionError( "schema-array-4.json", "Expected value of type INTEGER, but found '3.0'");
    }

  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. Schemas (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> array </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minItems </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maxItems </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.uniqueItems </TD> <TD> Boolean </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.items </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_5()
    {
    assertValidationFailure( "schema-array-5.json", "The value must be of integer type, but actual type is string");
    }

  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. Schemas (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> array </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minItems </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maxItems </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.uniqueItems </TD> <TD> Boolean </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.items </TD> <TD> <FONT color="red"> Empty-Schema  </FONT> </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_6()
    {
    assertDefinitionError( "schema-array-6.json", "Incomplete schema definition");
    }

  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. Schemas (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> array </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minItems </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maxItems </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.uniqueItems </TD> <TD> Boolean </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.items </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_7()
    {
    assertDefinitionError( "schema-array-7.json", "Schema has type=array but defines a 'const' of type=number");
    }

  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. Schemas (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> array </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minItems </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maxItems </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.uniqueItems </TD> <TD> Boolean </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.items </TD> <TD> <FONT color="red"> Invalid-Schema  </FONT> </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_8()
    {
    assertDefinitionError( "schema-array-8.json", "Unknown schema key '$ref'");
    }

  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9. Schemas (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> array </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minItems </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maxItems </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.uniqueItems </TD> <TD> Boolean </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.items </TD> <TD> <FONT color="red"> Invalid-Type  </FONT> </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_9()
    {
    assertValidationFailure( "schema-array-9.json", "The value must be of object type, but actual type is string");
    }

  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 10. Schemas (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> array </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minItems </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maxItems </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.uniqueItems </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.items </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_10()
    {
    // properties = valueSchema,valueTypeDefined

    // Given...
    //
    //   VarDef.Value.Schema.Defined = Yes
    //
    //   VarDef.Value.Schema.type = array
    //
    //   VarDef.Value.Schema.format = Undefined
    //
    //   VarDef.Value.Schema.const = Undefined
    //
    //   VarDef.Value.Schema.minItems = Undefined
    //
    //   VarDef.Value.Schema.maxItems = Integer
    //
    //   VarDef.Value.Schema.uniqueItems = Invalid
    //
    //   VarDef.Value.Schema.items = Undefined
    
    // When...

    // Then...
    }
  }
