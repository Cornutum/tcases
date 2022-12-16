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
 * Runs tests for input models that define "number" schemas.
 */
public class NumberSchemaTest extends SystemInputJsonTest
  {
  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Schemas (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maximum </TD> <TD> Number </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMinimum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMaximum </TD> <TD> Number </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.multipleOf </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_0()
    {
    expectSystemInputJson(
      "schema-number-0.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "number")
        .vars(
          VarDefBuilder.with( "numberVar")
          .values(
            VarValueDefBuilder.with( "numberValue")
            .schema(
              SchemaBuilder.type( "number")
              .maximum( "12.3")
              .exclusiveMinimum( 0)
              .exclusiveMaximum( "4.56")
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
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> number </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Defined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minimum </TD> <TD> Number </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMinimum </TD> <TD> Number </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMaximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.multipleOf </TD> <TD> Integer </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_1()
    {
    expectSystemInputJson(
      "schema-number-1.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "number")
        .vars(
          VarDefBuilder.with( "numberVar")
          .values(
            VarValueDefBuilder.with( "numberValue")
            .schema(
              SchemaBuilder.type( "number")
              .format( "float")
              .minimum( "-1.234")
              .exclusiveMinimum( "-12.34")
              .multipleOf( 1)
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
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> null </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.multipleOf </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_2()
    {
    expectSystemInputJson(
      "schema-number-2.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "number")
        .vars(
          VarDefBuilder.with( "numberVar")
          .values(
            VarValueDefBuilder.with( "numberValue")
            .schema(
              SchemaBuilder.generic()
              .format( "float")
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
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> number </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.multipleOf </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_3()
    {
    expectSystemInputJson(
      "schema-number-3.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "number")
        .vars(
          VarDefBuilder.with( "numberVar")
          .values(
            VarValueDefBuilder.with( "numberValue")
            .schema(
              SchemaBuilder.type( "number")
              .constant( 1234)
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
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Defined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minimum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMinimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMaximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.multipleOf </TD> <TD> Number </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_4()
    {
    expectSystemInputJson(
      "schema-number-4.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "number")
        .vars(
          VarDefBuilder.with( "numberVar")
          .values(
            VarValueDefBuilder.with( "numberValue")
            .schema(
              SchemaBuilder.type( "number")
              .minimum( 0)
              .maximum( 2147483647)
              .exclusiveMaximum( 2147483648L)
              .multipleOf( "1.1")
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
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> number </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Number </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.multipleOf </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_5()
    {
    expectSystemInputJson(
      "schema-number-5.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "number")
        .vars(
          VarDefBuilder.with( "numberVar")
          .values(
            VarValueDefBuilder.with( "numberValue")
            .schema(
              SchemaBuilder.type( "number")
              .constant( bigDecimalOf( "9223372036854775808"))
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
   * <TR align="left"><TH colspan=2> 6. Schemas (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Defined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maximum </TD> <TD> Number </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMinimum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMaximum </TD> <TD> Number </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.multipleOf </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_6()
    {
    assertDefinitionError(
      "schema-number-6.json",
      "Error processing Schema, number, numberVar, numberValue",
      "Schema has type=number but 'const' defines a value of type=string");
    }

  @Test
  public void whenValueTypeDefault()
    {
    expectSystemInputJson(
      "schema-number-default.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "number")
        .vars(
          VarDefBuilder.with( "numberVar")
          .schema( SchemaBuilder.type( "number").build())
          .values(
            VarValueDefBuilder.with( "numberValue")
            .schema(
              SchemaBuilder.type( "number")
              .exclusiveMaximum( 4)
              .build())
            .build())
          .build())
        .build())
      .build());
    }

  @Test
  public void Schemas_enum()
    {
    expectSystemInputJson(
      "schema-number-enum.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "number")
        .vars(
          VarDefBuilder.with( "numberVar")
          .values(
            VarValueDefBuilder.with( "numberValue")
            .schema(
              SchemaBuilder.type( "number")
              .enums( valueOf( bigDecimalOf( "2.3")), valueOf( 5), valueOf( 1), valueOf( 13), noValue())
              . build())
            .build())
          .build())
        .build())
      .build());
    }
  }
