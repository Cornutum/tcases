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
 * Runs tests for input models that define "string" schemas.
 */
public class StringSchemaTest extends SystemInputJsonTest
  {
  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Schemas (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Parent.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> VarDef.Parent.Schema.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.const </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.minLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.maxLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.pattern </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> string </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> date </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> String </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maxLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.pattern </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_0()
    {
    expectSystemInputJson(
      "schema-string-0.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "string")
        .vars(
          VarDefBuilder.with( "stringVar")
          .values(
            VarValueDefBuilder.with( "stringValue")
            .schema(
              SchemaBuilder.type( "string")
              .constant( "date", "2023-02-04")
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
   * <TR><TD> VarDef.Parent.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Parent.Schema.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.minLength </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.maxLength </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.pattern </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Other </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> null </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maxLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.pattern </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_1()
    {
    expectSystemInputJson(
      "schema-string-1.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "string")
        .vars(
          VarSetBuilder.with( "stringVarSet")
          .members(
            VarDefBuilder.with( "stringVar")
            .schema(
              SchemaBuilder.type( "string")
              .maxLength( 32)
              .build())
            .values(
              VarValueDefBuilder.with( "stringValue")
              .schema(
                SchemaBuilder.generic()
                .format( "uri")
                .constant( nullValue())
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
   * <TR align="left"><TH colspan=2> 2. Schemas (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Parent.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> VarDef.Parent.Schema.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.const </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.minLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.maxLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.pattern </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> string </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> uuid </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> null </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maxLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.pattern </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_2()
    {
    expectSystemInputJson(
      "schema-string-2.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "string")
        .vars(
          VarDefBuilder.with( "stringVar")
          .values(
            VarValueDefBuilder.with( "stringValue")
            .schema(
              SchemaBuilder.type( "string")
              .format( "uuid")
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
   * <TR align="left"><TH colspan=2> 3. Schemas (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Parent.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Parent.Schema.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> string </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> Defined </TD> </TR>
   * <TR><TD> VarDef.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.minLength </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.maxLength </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.pattern </TD> <TD> String </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> string </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> date-time </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minLength </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maxLength </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.pattern </TD> <TD> String </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_3()
    {
    expectSystemInputJson(
      "schema-string-3.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "string")
        .vars(
          VarSetBuilder.with( "stringVarSet")
          .members(
            VarDefBuilder.with( "stringVar")
            .schema(
              SchemaBuilder.type( "string")
              .format( "email")
              .minLength( 1)
              .pattern( "^[A-Z]+$")
              .build())
            .values(
              VarValueDefBuilder.with( "stringValue")
              .schema(
                SchemaBuilder.type( "string")
                .format( "date-time")
                .minLength( 32)
                .pattern( ".")
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
   * <TR align="left"><TH colspan=2> 4. Schemas (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Parent.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> VarDef.Parent.Schema.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.const </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.minLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.maxLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.pattern </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> email </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> String </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maxLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.pattern </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_4()
    {
    expectSystemInputJson(
      "schema-string-4.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "string")
        .vars(
          VarDefBuilder.with( "stringVar")
          .values(
            VarValueDefBuilder.with( "stringValue")
            .schema(
              SchemaBuilder.type( "string")
              .constant( "email", "me@myself.org")
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
   * <TR align="left"><TH colspan=2> 5. Schemas (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Parent.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Parent.Schema.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.minLength </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.maxLength </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.pattern </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minLength </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maxLength </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.pattern </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_5()
    {
    expectSystemInputJson(
      "schema-string-5.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "string")
        .vars(
          VarSetBuilder.with( "stringVarSet")
          .members(
            VarDefBuilder.with( "stringVar")
            .schema(
              SchemaBuilder.type( "string")
              .maxLength( 32)
              .build())
            .values(
              VarValueDefBuilder.with( "stringValue")
              .schema(
                SchemaBuilder.type( "string")
                .pattern( "^[0-9]$*")
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
   * <TR align="left"><TH colspan=2> 6. Schemas (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Parent.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> VarDef.Parent.Schema.Defined </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.const </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.minLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.maxLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.pattern </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maxLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.pattern </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_6()
    {
    expectSystemInputJson(
      "schema-string-6.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "string")
        .vars(
          VarDefBuilder.with( "stringVar")
          .values(
            VarValueDefBuilder.with( "stringValue")
            .build())
          .build())
        .build())
      .build());
    }

  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. Schemas (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Parent.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Parent.Schema.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> string </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> Defined </TD> </TR>
   * <TR><TD> VarDef.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.minLength </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.maxLength </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.pattern </TD> <TD> String </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> string </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> date </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minLength </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maxLength </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.pattern </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_7()
    {
    // properties = valueSchema,varParent,varSchema

    // Given...
    //
    //   VarDef.Parent.Defined = Yes
    //
    //   VarDef.Parent.Schema.Defined = No
    //
    //   VarDef.Schema.Defined = Yes
    //
    //   VarDef.Schema.type = string
    //
    //   VarDef.Schema.format = Defined
    //
    //   VarDef.Schema.const = Undefined
    //
    //   VarDef.Schema.minLength = Integer
    //
    //   VarDef.Schema.maxLength = Undefined
    //
    //   VarDef.Schema.pattern = String
    //
    //   VarDef.Value.Schema.Defined = Yes
    //
    //   VarDef.Value.Schema.type = string
    //
    //   VarDef.Value.Schema.format = date
    //
    //   VarDef.Value.Schema.const = Undefined
    //
    //   VarDef.Value.Schema.minLength = Integer
    //
    //   VarDef.Value.Schema.maxLength = Undefined
    //
    //   VarDef.Value.Schema.pattern = Invalid
    
    // When...

    // Then...
    }

  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. Schemas (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Parent.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Parent.Schema.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> string </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> Defined </TD> </TR>
   * <TR><TD> VarDef.Schema.const </TD> <TD> <FONT color="red"> Defined  </FONT> </TD> </TR>
   * <TR><TD> VarDef.Schema.minLength </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.maxLength </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.pattern </TD> <TD> String </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> string </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> date </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> String </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maxLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.pattern </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_8()
    {
    // properties = valueConst,valueSchema,varParent,varSchema

    // Given...
    //
    //   VarDef.Parent.Defined = Yes
    //
    //   VarDef.Parent.Schema.Defined = No
    //
    //   VarDef.Schema.Defined = Yes
    //
    //   VarDef.Schema.type = string
    //
    //   VarDef.Schema.format = Defined
    //
    //   VarDef.Schema.const = Defined
    //
    //   VarDef.Schema.minLength = Integer
    //
    //   VarDef.Schema.maxLength = Undefined
    //
    //   VarDef.Schema.pattern = String
    //
    //   VarDef.Value.Schema.Defined = Yes
    //
    //   VarDef.Value.Schema.type = string
    //
    //   VarDef.Value.Schema.format = date
    //
    //   VarDef.Value.Schema.const = String
    //
    //   VarDef.Value.Schema.minLength = (not applicable)
    //
    //   VarDef.Value.Schema.maxLength = (not applicable)
    //
    //   VarDef.Value.Schema.pattern = (not applicable)
    
    // When...

    // Then...
    }

  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9. Schemas (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Parent.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Parent.Schema.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> string </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> Defined </TD> </TR>
   * <TR><TD> VarDef.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.minLength </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.maxLength </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.pattern </TD> <TD> String </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> string </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> String </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maxLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.pattern </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_9()
    {
    // properties = valueConst,valueSchema,varParent,varSchema

    // Given...
    //
    //   VarDef.Parent.Defined = Yes
    //
    //   VarDef.Parent.Schema.Defined = No
    //
    //   VarDef.Schema.Defined = Yes
    //
    //   VarDef.Schema.type = string
    //
    //   VarDef.Schema.format = Defined
    //
    //   VarDef.Schema.const = Undefined
    //
    //   VarDef.Schema.minLength = Integer
    //
    //   VarDef.Schema.maxLength = Undefined
    //
    //   VarDef.Schema.pattern = String
    //
    //   VarDef.Value.Schema.Defined = Yes
    //
    //   VarDef.Value.Schema.type = string
    //
    //   VarDef.Value.Schema.format = Invalid
    //
    //   VarDef.Value.Schema.const = String
    //
    //   VarDef.Value.Schema.minLength = (not applicable)
    //
    //   VarDef.Value.Schema.maxLength = (not applicable)
    //
    //   VarDef.Value.Schema.pattern = (not applicable)
    
    // When...

    // Then...
    }

  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 10. Schemas (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Parent.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Parent.Schema.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> string </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> Defined </TD> </TR>
   * <TR><TD> VarDef.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.minLength </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.maxLength </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.pattern </TD> <TD> String </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> string </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> date </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minLength </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maxLength </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.pattern </TD> <TD> String </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_10()
    {
    // properties = valueSchema,varParent,varSchema

    // Given...
    //
    //   VarDef.Parent.Defined = Yes
    //
    //   VarDef.Parent.Schema.Defined = No
    //
    //   VarDef.Schema.Defined = Yes
    //
    //   VarDef.Schema.type = string
    //
    //   VarDef.Schema.format = Defined
    //
    //   VarDef.Schema.const = Undefined
    //
    //   VarDef.Schema.minLength = Integer
    //
    //   VarDef.Schema.maxLength = Undefined
    //
    //   VarDef.Schema.pattern = String
    //
    //   VarDef.Value.Schema.Defined = Yes
    //
    //   VarDef.Value.Schema.type = string
    //
    //   VarDef.Value.Schema.format = date
    //
    //   VarDef.Value.Schema.const = Undefined
    //
    //   VarDef.Value.Schema.minLength = Invalid
    //
    //   VarDef.Value.Schema.maxLength = Undefined
    //
    //   VarDef.Value.Schema.pattern = String
    
    // When...

    // Then...
    }

  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 11. Schemas (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Parent.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Parent.Schema.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> string </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> Defined </TD> </TR>
   * <TR><TD> VarDef.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.minLength </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.maxLength </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.pattern </TD> <TD> String </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> date </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> null </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maxLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.pattern </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_11()
    {
    // properties = valueConst,valueSchema,varParent,varSchema

    // Given...
    //
    //   VarDef.Parent.Defined = Yes
    //
    //   VarDef.Parent.Schema.Defined = No
    //
    //   VarDef.Schema.Defined = Yes
    //
    //   VarDef.Schema.type = string
    //
    //   VarDef.Schema.format = Defined
    //
    //   VarDef.Schema.const = Undefined
    //
    //   VarDef.Schema.minLength = Integer
    //
    //   VarDef.Schema.maxLength = Undefined
    //
    //   VarDef.Schema.pattern = String
    //
    //   VarDef.Value.Schema.Defined = Yes
    //
    //   VarDef.Value.Schema.type = Invalid
    //
    //   VarDef.Value.Schema.format = date
    //
    //   VarDef.Value.Schema.const = null
    //
    //   VarDef.Value.Schema.minLength = (not applicable)
    //
    //   VarDef.Value.Schema.maxLength = (not applicable)
    //
    //   VarDef.Value.Schema.pattern = (not applicable)
    
    // When...

    // Then...
    }

  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 12. Schemas (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Parent.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Parent.Schema.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> string </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> Defined </TD> </TR>
   * <TR><TD> VarDef.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.minLength </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.maxLength </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.pattern </TD> <TD> String </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> string </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> date </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minLength </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maxLength </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.pattern </TD> <TD> String </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_12()
    {
    // properties = valueSchema,varParent,varSchema

    // Given...
    //
    //   VarDef.Parent.Defined = Yes
    //
    //   VarDef.Parent.Schema.Defined = No
    //
    //   VarDef.Schema.Defined = Yes
    //
    //   VarDef.Schema.type = string
    //
    //   VarDef.Schema.format = Defined
    //
    //   VarDef.Schema.const = Undefined
    //
    //   VarDef.Schema.minLength = Integer
    //
    //   VarDef.Schema.maxLength = Undefined
    //
    //   VarDef.Schema.pattern = String
    //
    //   VarDef.Value.Schema.Defined = Yes
    //
    //   VarDef.Value.Schema.type = string
    //
    //   VarDef.Value.Schema.format = date
    //
    //   VarDef.Value.Schema.const = Undefined
    //
    //   VarDef.Value.Schema.minLength = Integer
    //
    //   VarDef.Value.Schema.maxLength = Invalid
    //
    //   VarDef.Value.Schema.pattern = String
    
    // When...

    // Then...
    }

  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 13. Schemas (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Parent.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Parent.Schema.Defined </TD> <TD> <FONT color="red"> Yes  </FONT> </TD> </TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> string </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> Defined </TD> </TR>
   * <TR><TD> VarDef.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.minLength </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.maxLength </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.pattern </TD> <TD> String </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> string </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> date </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> String </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maxLength </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.pattern </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_13()
    {
    // properties = valueConst,valueSchema,varParent,varSchema

    // Given...
    //
    //   VarDef.Parent.Defined = Yes
    //
    //   VarDef.Parent.Schema.Defined = Yes
    //
    //   VarDef.Schema.Defined = Yes
    //
    //   VarDef.Schema.type = string
    //
    //   VarDef.Schema.format = Defined
    //
    //   VarDef.Schema.const = Undefined
    //
    //   VarDef.Schema.minLength = Integer
    //
    //   VarDef.Schema.maxLength = Undefined
    //
    //   VarDef.Schema.pattern = String
    //
    //   VarDef.Value.Schema.Defined = Yes
    //
    //   VarDef.Value.Schema.type = string
    //
    //   VarDef.Value.Schema.format = date
    //
    //   VarDef.Value.Schema.const = String
    //
    //   VarDef.Value.Schema.minLength = (not applicable)
    //
    //   VarDef.Value.Schema.maxLength = (not applicable)
    //
    //   VarDef.Value.Schema.pattern = (not applicable)
    
    // When...

    // Then...
    }

  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 14. Schemas (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Parent.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Parent.Schema.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> string </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> Defined </TD> </TR>
   * <TR><TD> VarDef.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.minLength </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.maxLength </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.pattern </TD> <TD> String </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> string </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> date </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minLength </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maxLength </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.pattern </TD> <TD> String </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_14()
    {
    // properties = valueSchema,varParent,varSchema

    // Given...
    //
    //   VarDef.Parent.Defined = Yes
    //
    //   VarDef.Parent.Schema.Defined = No
    //
    //   VarDef.Schema.Defined = Yes
    //
    //   VarDef.Schema.type = string
    //
    //   VarDef.Schema.format = Defined
    //
    //   VarDef.Schema.const = Undefined
    //
    //   VarDef.Schema.minLength = Integer
    //
    //   VarDef.Schema.maxLength = Undefined
    //
    //   VarDef.Schema.pattern = String
    //
    //   VarDef.Value.Schema.Defined = Yes
    //
    //   VarDef.Value.Schema.type = string
    //
    //   VarDef.Value.Schema.format = date
    //
    //   VarDef.Value.Schema.const = Invalid
    //
    //   VarDef.Value.Schema.minLength = Integer
    //
    //   VarDef.Value.Schema.maxLength = Undefined
    //
    //   VarDef.Value.Schema.pattern = String
    
    // When...

    // Then...
    }
  }
