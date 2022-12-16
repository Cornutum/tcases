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
 * Runs tests for input models that define "integer" schemas.
 */
public class IntegerSchemaTest extends SystemInputJsonTest
  {
  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Schemas (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> integer </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.minimum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.maximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMinimum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMaximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.multipleOf </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Undefined </TD> </TR>
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
  public void Schemas_0()
    {
    expectSystemInputJson(
      "schema-integer-0.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "integer")
        .vars(
          VarDefBuilder.with( "integerVar")
          .schema(
            SchemaBuilder.type( "integer")
            .minimum( 100)
            .exclusiveMinimum( 100)
            .build())
          .values(
            VarValueDefBuilder.with( "integerValue")
            .schema(
              SchemaBuilder.generic()
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
   * <TR align="left"><TH colspan=2> 1. Schemas (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.minimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.maximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.multipleOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> int64 </TD> </TR>
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
  public void Schemas_1()
    {
    expectSystemInputJson(
      "schema-integer-1.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "integer")
        .vars(
          VarDefBuilder.with( "integerVar")
          .values(
            VarValueDefBuilder.with( "integerValue")
            .schema(
              SchemaBuilder.type( "integer")
              .format( "int64")
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
   * <TR align="left"><TH colspan=2> 2. Schemas (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> Defined </TD> </TR>
   * <TR><TD> VarDef.Schema.minimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.maximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMinimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMaximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.multipleOf </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> Undefined </TD> </TR>
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
  public void Schemas_2()
    {
    expectSystemInputJson(
      "schema-integer-2.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "integer")
        .vars(
          VarDefBuilder.with( "integerVar")
          .schema(
            SchemaBuilder.type( "integer")
            .format( "int64")
            .maximum( 2147483648L)
            .exclusiveMaximum( 2147483647)
            .multipleOf( 2)
            .build())
          .values(
            VarValueDefBuilder.with( "integerValue")
            .schema(
              SchemaBuilder.type( "integer")
              .constant( 1001)
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
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.minimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.maximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.multipleOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Undefined </TD> </TR>
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
  public void Schemas_3()
    {
    expectSystemInputJson(
      "schema-integer-3.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "integer")
        .vars(
          VarDefBuilder.with( "integerVar")
          .values(
            VarValueDefBuilder.with( "integerValue")
            .schema(
              SchemaBuilder.type( "integer")
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
   * <TR align="left"><TH colspan=2> 4. Schemas (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> integer </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.minimum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.maximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMinimum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMaximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.multipleOf </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> int64 </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minimum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMinimum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMaximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.multipleOf </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_4()
    {
    expectSystemInputJson(
      "schema-integer-4.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "integer")
        .vars(
          VarDefBuilder.with( "integerVar")
          .schema(
            SchemaBuilder.type( "integer")
            .minimum( 100)
            .exclusiveMinimum( 100)
            .build())
          .values(
            VarValueDefBuilder.with( "integerValue")
            .schema(
              SchemaBuilder.type( "integer")
              .format( "int64")
              .minimum( 0)
              .exclusiveMaximum( 200)
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
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.minimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.maximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.multipleOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Other </TD> </TR>
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
  public void Schemas_5()
    {
    expectSystemInputJson(
      "schema-integer-5.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "integer")
        .vars(
          VarDefBuilder.with( "integerVar")
          .values(
            VarValueDefBuilder.with( "integerValue")
            .schema(
              SchemaBuilder.type( "integer")
              .format( "base2")
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
   * <TR align="left"><TH colspan=2> 6. Schemas (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> Defined </TD> </TR>
   * <TR><TD> VarDef.Schema.minimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.maximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMinimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMaximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.multipleOf </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> int32 </TD> </TR>
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
  public void Schemas_6()
    {
    expectSystemInputJson(
      "schema-integer-6.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "integer")
        .vars(
          VarDefBuilder.with( "integerVar")
          .schema(
            SchemaBuilder.type( "integer")
            .format( "int64")
            .maximum( 100)
            .exclusiveMaximum( 100)
            .multipleOf( 5)
            .build())
          .values(
            VarValueDefBuilder.with( "integerValue")
            .schema(
              SchemaBuilder.type( "integer")
              .format( "int32")
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
   * <TR align="left"><TH colspan=2> 7. Schemas (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.minimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.maximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.multipleOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Other </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMinimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMaximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.multipleOf </TD> <TD> Integer </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_7()
    {
    expectSystemInputJson(
      "schema-integer-7.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "integer")
        .vars(
          VarDefBuilder.with( "integerVar")
          .values(
            VarValueDefBuilder.with( "integerValue")
            .schema(
              SchemaBuilder.type( "number")
              .format( "base2")
              .maximum( 125)
              .multipleOf( 5)
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
   * <TR align="left"><TH colspan=2> 8. Schemas (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> integer </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.minimum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.maximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMinimum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMaximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.multipleOf </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minimum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMinimum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMaximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.multipleOf </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_8()
    {
    expectSystemInputJson(
      "schema-integer-8.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "integer")
        .vars(
          VarDefBuilder.with( "integerVar")
          .schema(
            SchemaBuilder.type( "integer")
            .minimum( 100)
            .exclusiveMinimum( 100)
            .build())
          .values(
            VarValueDefBuilder.with( "integerValue")
            .schema(
              SchemaBuilder.type( "integer")
              .minimum( 234)
              .exclusiveMinimum( 456)
              .exclusiveMaximum( 789)
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
   * <TR align="left"><TH colspan=2> 9. Schemas (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.minimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.maximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.multipleOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> int32 </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMinimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMaximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.multipleOf </TD> <TD> Integer </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_9()
    {
    expectSystemInputJson(
      "schema-integer-9.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "integer")
        .vars(
          VarDefBuilder.with( "integerVar")
          .values(
            VarValueDefBuilder.with( "integerValue")
            .schema(
              SchemaBuilder.type( "integer")
              .format( "int32")
              .maximum( 234)
              .multipleOf( 2)
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
   * <TR align="left"><TH colspan=2> 10. Schemas (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> Defined </TD> </TR>
   * <TR><TD> VarDef.Schema.minimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.maximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMinimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMaximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.multipleOf </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> int64 </TD> </TR>
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
  public void Schemas_10()
    {
    expectSystemInputJson(
      "schema-integer-10.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "integer")
        .vars(
          VarDefBuilder.with( "integerVar")
          .schema(
            SchemaBuilder.type( "integer")
            .format( "int32")
            .maximum( 32)
            .exclusiveMaximum( 32)
            .multipleOf( 4)
            .build())
          .values(
            VarValueDefBuilder.with( "integerValue")
            .schema(
              SchemaBuilder.type( "integer")
              .format( "int64")
              .constant( -123L)
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
   * <TR align="left"><TH colspan=2> 11. Schemas (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.minimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.maximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Schema.multipleOf </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> int32 </TD> </TR>
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
  public void Schemas_11()
    {
    expectSystemInputJson(
      "schema-integer-11.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "integer")
        .vars(
          VarDefBuilder.with( "integerVar")
          .values(
            VarValueDefBuilder.with( "integerValue")
            .schema(
              SchemaBuilder.type( "integer")
              .format( "int32")
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
   * <TR align="left"><TH colspan=2> 12. Schemas (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> integer </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.minimum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.maximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMinimum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMaximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.multipleOf </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Other </TD> </TR>
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
  public void Schemas_12()
    {
    expectSystemInputJson(
      "schema-integer-12.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "integer")
        .vars(
          VarDefBuilder.with( "integerVar")
          .schema(
            SchemaBuilder.type( "integer")
            .minimum( 100)
            .exclusiveMinimum( 100)
            .build())
          .values(
            VarValueDefBuilder.with( "integerValue")
            .schema(
              SchemaBuilder.type( "integer")
              .format( "base2")
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
   * <TR align="left"><TH colspan=2> 14. Schemas (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> Defined </TD> </TR>
   * <TR><TD> VarDef.Schema.minimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.maximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMinimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMaximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.multipleOf </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minimum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMinimum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMaximum </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.multipleOf </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_14()
    {
    assertValidationFailure( "schema-integer-14.json", "The value must be of number type, but actual type is boolean");
    }

  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 15. Schemas (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> Defined </TD> </TR>
   * <TR><TD> VarDef.Schema.minimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.maximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMinimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMaximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.multipleOf </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minimum </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMinimum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMaximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.multipleOf </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_15()
    {
    assertValidationFailure( "schema-integer-15.json", "The value must be of number type, but actual type is string");
    }

  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 16. Schemas (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> Defined </TD> </TR>
   * <TR><TD> VarDef.Schema.minimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.maximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMinimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMaximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.multipleOf </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minimum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMinimum </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMaximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.multipleOf </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_16()
    {
    assertValidationFailure( "schema-integer-16.json", "The value must be of number type, but actual type is null");
    }

  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 17. Schemas (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> Defined </TD> </TR>
   * <TR><TD> VarDef.Schema.minimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.maximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMinimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMaximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.multipleOf </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minimum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMinimum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMaximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.multipleOf </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_17()
    {
    assertValidationFailure( "schema-integer-17.json", "The value must be of number type, but actual type is string");
    }

  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 18. Schemas (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> Defined </TD> </TR>
   * <TR><TD> VarDef.Schema.minimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.maximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMinimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMaximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.multipleOf </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minimum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maximum </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMinimum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMaximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.multipleOf </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_18()
    {
    assertValidationFailure( "schema-integer-18.json", "The value must be of number type, but actual type is null");
    }

  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 19. Schemas (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> Defined </TD> </TR>
   * <TR><TD> VarDef.Schema.minimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.maximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMinimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMaximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.multipleOf </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minimum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMinimum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMaximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.multipleOf </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_19()
    {
    assertDefinitionError(
      "schema-integer-19.json",
      "Error processing Schema, integer, integerVar, integerValue, const",
      "Expected an int32 value but found 2147483648");
    }

  /**
   * Tests input models using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 19. Schemas (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> VarDef.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.format </TD> <TD> Defined </TD> </TR>
   * <TR><TD> VarDef.Schema.minimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.maximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMinimum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Schema.exclusiveMaximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Schema.multipleOf </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.type </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.format </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.const </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.minimum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.maximum </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMinimum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.exclusiveMaximum </TD> <TD> Integer </TD> </TR>
   * <TR><TD> VarDef.Value.Schema.multipleOf </TD> <TD> Undefined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Schemas_20()
    {
    assertDefinitionError(
      "schema-integer-20.json",
      "Error processing Schema, integer, integerVar, integerValue",
      "Schema has type=integer but 'const' defines a value of type=number");
    }

  
  @Test
  public void whenValueTypeDefault()
    {
    expectSystemInputJson(
      "schema-integer-default.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "integer")
        .vars(
          VarDefBuilder.with( "integerVar")
          .schema(
            SchemaBuilder.type( "integer")
            .minimum( 100)
            .exclusiveMinimum( 100)
            .build())
          .values(
            VarValueDefBuilder.with( "integerValue")
            .schema(
              SchemaBuilder.type( "integer")
              .multipleOf( 3)
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
      "schema-integer-enum.json",
      
      SystemInputDefBuilder.with( "Schema")
      .functions(
        FunctionInputDefBuilder.with( "integer")
        .vars(
          VarDefBuilder.with( "integerVar")
          .values(
            VarValueDefBuilder.with( "integerValue")
            .schema(
              SchemaBuilder.type( "integer")
              .enums( 1, 2, 3, 5, 8, 13)
              .build())
            .build())
          .build())
        .build())
      .build());
    }

  @Test
  public void Schemas_enum_invalid()
    {
    assertDefinitionError(
      "schema-integer-enum-invalid.json",
      "Error processing Schema, integer, integerVar, integerValue",
      "Schema has type=integer but 'enum' defines a value of type=number");
    }
  }
