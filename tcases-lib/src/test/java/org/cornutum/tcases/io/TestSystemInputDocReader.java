//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.*;
import static org.cornutum.tcases.conditions.Conditions.*;
import static org.cornutum.tcases.util.Asserts.*;
import static org.cornutum.tcases.VarValueDef.Type.*;

import org.junit.Test;
import static org.junit.Assert.*;

import java.math.BigDecimal;

import org.xml.sax.SAXParseException;

/**
 * Runs tests for the {@link SystemInputDocReader}.
 *
 */
public class TestSystemInputDocReader
  {
  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0.   getSystemInputDef (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> None </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> No </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> Many </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_0()
    {
    // Given...
    SystemInputDef expected =
      SystemInputDefBuilder.with( "System-0")
      .has( "A0", "AV0")

      .functions(
        FunctionInputDefBuilder.with( "Function-0")
        .vars(
          "state",

          VarDefBuilder.with( "state-0-0")
          .when( allOf( has( "R1"), not( "R2", "R3")))
          .values(
            VarValueDefBuilder.with( "value-0")
            .when( allOf( has( "R4"), not( "R5", "R1")))
            .properties( "P1", "P2", "P3", "P4", "P5")
            .build())
          .build(),

          VarSetBuilder.with( "state-0-1")
          .when( has( "P1"))
          .members(
            VarDefBuilder.with( "member-0")
            .when( allOf( has( "P2"), not( "P3", "P4")))
            .values(
              VarValueDefBuilder.with( "value-0")
              .when( allOf( has( "P5"), not( "P3", "P4")))
              .properties( "R1", "R2", "R3", "R4", "R5")
              .build())
            .build(),

            VarSetBuilder.with( "member-1")
            .members(
              VarDefBuilder.with( "member-1-0")
              .values(
                VarValueDefBuilder.with( "value-1-0").build())
              .build(),
              
              VarDefBuilder.with( "member-1-1")
              .values(
                VarValueDefBuilder.with( "value-1-1").build())
              .build())
            .build())
          .build())
        .build())
      .build();               

    // When...
    SystemInputDef systemInputDef = systemInputResources_.read( "system-input-def-0.xml");

    // Then...
    assertMatches( "system-input-def-0.xml", expected, systemInputDef, Matchers.systemInputDefMatcher);
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1.   getSystemInputDef (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_1()
    {
    // Given...
    SystemInputDef expected =
      SystemInputDefBuilder.with( "System-1")
      .build();

    // When...
    SystemInputDef systemInputDef = systemInputResources_.read( "system-input-def-1.xml");

    // Then...
    assertMatches( "system-input-def-1.xml", expected, systemInputDef, Matchers.systemInputDefMatcher); 
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2.   getSystemInputDef (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_2()
    {
    // Given...
    SystemInputDef expected =
      SystemInputDefBuilder.with( "System-2")
      .has( "A0", "AV0")
      .has( "A1", "AV1")
      .functions(

        FunctionInputDefBuilder.with( "Function-0")
        .has( "A0", "AV0")
        .has( "A1", "AV1")
        .vars(
          "arg",
          VarDefBuilder.with( "arg-0-0")
          .when( allOf( has( "E1", "E2", "E3"), not( "E4")))
          .values(
            VarValueDefBuilder.with( "value-0")
            .when( allOf( has( "E5", "E6"), not( "E7")))
            .properties( "A1", "A2", "A3", "A4", "A5", "A6")
            .build(),

            VarValueDefBuilder.with( "value-1")
            .when( allOf( has( "E8", "E9"), not( "E10")))
            .type( FAILURE)
            .build(),

            VarValueDefBuilder.with( "value-2")
            .when( allOf( has( "E11", "E12"), not( "E1")))
            .type( ONCE)
            .properties( "A7", "A8", "A9", "A10", "A11", "A12")
            .build())
          .build(),
          
          VarDefBuilder.with( "arg-0-1")
          .values(
            VarValueDefBuilder.with( "value-3")
            .type( ONCE)
            .build(),

            VarValueDefBuilder.with( "value-4")
            .build(),

            VarValueDefBuilder.with( "value-5")
            .type( FAILURE)
            .build())
          .build(),

          VarSetBuilder.with( "arg-0-2")
          .members(
            VarDefBuilder.with( "member-0")
            .values(
              VarValueDefBuilder.with( "value-6")
              .build())
            .build())
          .build())

        .vars(
          "env",
          VarSetBuilder.with( "env-0-0")
          .when( not( "A1"))
          .members(
            VarDefBuilder.with( "member-0")
            .values(
              VarValueDefBuilder.with( "value-7")
              .build(),

              VarValueDefBuilder.with( "value-8")
              .build())
            .build())
          .build(),

          VarSetBuilder.with( "env-0-1")
          .when( not( "A2"))
          .members(
            VarDefBuilder.with( "member-0")
            .when( allOf( has( "A3", "A4", "A5"), not( "A6")))
            .values(
              VarValueDefBuilder.with( "value-9")
              .when( allOf( has( "A7", "A8"), not( "A9")))
              .type( FAILURE)
              .build(),

              VarValueDefBuilder.with( "value-10")
              .when( allOf( has( "A10", "A11"), not( "A12")))
              .properties( "E1", "E2", "E3", "E4", "E5", "E6")
              .build(),

              VarValueDefBuilder.with( "value-11")
              .when( allOf( has( "A12", "A11"), not( "A10")))
              .type( ONCE)
              .properties( "E7", "E8", "E9", "E10", "E11", "E12")
              .build())
            .build())
          .build(),

          VarDefBuilder.with( "env-0-2")
            .values(
              VarValueDefBuilder.with( "value-12")
              .build())
          .build())
        .build(),

        FunctionInputDefBuilder.with( "Function-1")
        .vars(
          "arg",
          VarDefBuilder.with( "arg-1-1")
          .values(
            VarValueDefBuilder.with( "value-14")
            .build())
          .build(),

          VarSetBuilder.with( "arg-1-0")
          .members(
            VarDefBuilder.with( "member-0")
            .values(
              VarValueDefBuilder.with( "value-13")
              .build())
            .build())
          .build())
        .build())
        
      .build();

    // When...
    SystemInputDef systemInputDef = systemInputResources_.read( "system-input-def-2.xml");

    // Then...
    assertMatches( "system-input-def-2.xml", expected, systemInputDef, Matchers.systemInputDefMatcher); 
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3.   getSystemInputDef (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_3()
    {
    // Given...
    SystemInputDef expected =
      SystemInputDefBuilder.with( "System-3")
      .functions(
        FunctionInputDefBuilder.with( "Function-0")
        .has( "A0", "AV0")
        .has( "A1", "AV1")
        .vars(
          "arg",
          VarSetBuilder.with( "arg-0-0")
          .members(
            VarDefBuilder.with( "member-0")
            .values(
              VarValueDefBuilder.with( "value_0")
              .build())
            .build())
          .build(),
          
          VarSetBuilder.with( "arg-0-1")
          .members(
            VarDefBuilder.with( "member-0")
            .values(
              VarValueDefBuilder.with( "value_1")
              .build())
            .build())
          .build())
        
        .vars(
          "env",
          VarSetBuilder.with( "env-0-0")
          .members(
            VarDefBuilder.with( "member-0")
            .values(
              VarValueDefBuilder.with( "value_2")
              .build())
            .build())
          .build(),
          
          VarSetBuilder.with( "env-0-1")
          .members(
            VarDefBuilder.with( "member-0")
            .values(
              VarValueDefBuilder.with( "value_3")
              .build())
            .build())
          .build())
        .build(),

        FunctionInputDefBuilder.with( "Function-1")
        .vars(
          "arg",
          VarSetBuilder.with( "arg-1-0")
          .members(
            VarDefBuilder.with( "member-0")
            .values(
              VarValueDefBuilder.with( "value_4")
              .build())
            .build())
          .build(),
          
          VarSetBuilder.with( "arg-1-1")
          .members(
            VarDefBuilder.with( "member-0")
            .values(
              VarValueDefBuilder.with( "value_5")
              .build())
            .build())
          .build())
        
        .vars(
          "env",
          VarSetBuilder.with( "env-1-0")
          .members(
            VarDefBuilder.with( "member-0")
            .values(
              VarValueDefBuilder.with( "value_6")
              .build())
            .build())
          .build(),
          
          VarSetBuilder.with( "env-1-1")
          .members(
            VarDefBuilder.with( "member-0")
            .values(
              VarValueDefBuilder.with( "value_7")
              .build())
            .build())
          .build())
        .build())
      .build();

    // When...
    SystemInputDef systemInputDef = systemInputResources_.read( "system-input-def-3.xml");

    // Then...
    assertMatches( "system-input-def-3.xml", expected, systemInputDef, Matchers.systemInputDefMatcher); 
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4.   getSystemInputDef (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> AnyOf </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> AnyOf </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> None </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_4()
    {
    // Given...
    SystemInputDef expected =
      SystemInputDefBuilder.with( "System-4")
      .functions(
        FunctionInputDefBuilder.with( "Function-0")
        .has( "A0", "AV0")
        .vars(
          "state",
          VarDefBuilder.with( "state-0-0")
          .when( anyOf( hasAny( "R1")))
          .values(
            VarValueDefBuilder.with( "value-0")
            .type( FAILURE)
            .when( anyOf( hasAny( "R2")))
            .build(),

            VarValueDefBuilder.with( "value-1")
            .type( ONCE)
            .when( anyOf( hasAny( "R3")))
            .properties( "P1", "P5")
            .build(),

            VarValueDefBuilder.with( "value-2")
            .type( FAILURE)
            .when( anyOf( hasAny( "R4")))
            .build(),
            
            VarValueDefBuilder.with( "value-3")
            .when( anyOf( hasAny( "R5")))
            .properties( "P2", "P3", "P4")
            .build())
          .build(),

          VarDefBuilder.with( "state-0-1")
          .values(
            VarValueDefBuilder.with( "value-4")
            .properties( "R1", "R2", "R3", "R4", "R5")
            .build())
          .build())
        .build(),
        
        FunctionInputDefBuilder.with( "Function-1")
        .vars(
          "state",
          VarDefBuilder.with( "state-1-0")
          .has( "A1", "AV1")
          .when( anyOf( hasAny( "P1")))
          .values(
            VarValueDefBuilder.with( "value-0")
            .when( anyOf( hasAny( "P5")))
            .properties( "R1", "R2")
            .build(),

            VarValueDefBuilder.with( "value-1")
            .type( ONCE)
            .when( anyOf( hasAny( "P3")))
            .properties( "R3", "R4", "R5")
            .build(),

            VarValueDefBuilder.with( "value-2")
            .type( FAILURE)
            .when( anyOf( hasAny( "P4")))
            .build(),
            
            VarValueDefBuilder.with( "value-3")
            .type( FAILURE)
            .when( anyOf( hasAny( "P2")))
            .build())
          .build(),

          VarDefBuilder.with( "state-1-1")
          .has( "A1", "AV1")
          .values(
            VarValueDefBuilder.with( "value-4")
            .properties( "P1", "P2", "P3", "P4", "P5")
            .build())
          .build())
        .build())
      .build();

    // When...
    SystemInputDef systemInputDef = systemInputResources_.read( "system-input-def-4.xml");

    // Then...
    assertMatches( "system-input-def-4.xml", expected, systemInputDef, Matchers.systemInputDefMatcher); 
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5.   getSystemInputDef (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> Many </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_5()
    {
    // Given...
    SystemInputDef expected =
      SystemInputDefBuilder.with( "System-5")
      .functions(
        FunctionInputDefBuilder.with( "Function-0")
        .vars(
          "env",
          VarDefBuilder.with( "env-0-0")
          .has( "A0", "AV0")
          .when( allOf( has( "P1", "P2", "P3"), not( "P4"), allOf( "P5"), anyOf( "P6")))
          .values(
            VarValueDefBuilder.with( "value-0")
            .has( "A5", "AV5")
            .has( "A6", "AV6")
            .type( FAILURE)
            .when( allOf( has( "P1", "P2", "P3"), not( "P4"), allOf( "P5"), anyOf( "P6")))
            .build(),

            VarValueDefBuilder.with( "value-1")
            .type( ONCE)
            .when( allOf( has( "P1", "P2", "P3"), not( "P4"), allOf( "P5"), anyOf( "P6")))
            .build())
          .build(),

          VarDefBuilder.with( "env-0-1")
          .when( allOf( has( "P1", "P2", "P3"), not( "P4"), allOf( "P5"), anyOf( "P6")))
          .values(
            VarValueDefBuilder.with( "value-2")
            .type( FAILURE)
            .when( allOf( has( "P1", "P2", "P3"), not( "P4"), allOf( "P5"), anyOf( "P6")))
            .build(),

            VarValueDefBuilder.with( "value-3")
            .type( ONCE)
            .when( allOf( has( "P1", "P2", "P3"), not( "P4"), allOf( "P5"), anyOf( "P6")))
            .build())
          .build(),

          VarSetBuilder.with( "env-0-2")
          .has( "A1", "AV1")
          .members(
            VarDefBuilder.with( "member-0")
            .has( "A2", "AV2")
            .has( "A3", "AV3")
            .values(
              VarValueDefBuilder.with( "value-4")
              .properties( "P1", "P2", "P3")
              .build())
            .build())
          .build(),

          VarSetBuilder.with( "env-0-3")
          .has( "A7", "AV7")
          .has( "A8", "AV8")
          .members(
            VarDefBuilder.with( "member-0")
            .values(
              VarValueDefBuilder.with( "value-5")
              .has( "A4", "AV4")
              .properties( "P4", "P5", "P6")
              .build())
            .build())
          .build())
        .build())
      .build();

    // When...
    SystemInputDef systemInputDef = systemInputResources_.read( "system-input-def-5.xml");

    // Then...
    assertMatches( "system-input-def-5.xml", expected, systemInputDef, Matchers.systemInputDefMatcher); 
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6.   getSystemInputDef (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Undefined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> Many </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> One </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_6()
    {
    // Given...
    SystemInputDef expected =
      SystemInputDefBuilder.with( "System-6")
      .functions(
        FunctionInputDefBuilder.with( "Function-0")
        .vars(
          "arg",
          VarSetBuilder.with( "arg-0-0")
          .when( not( anyOf( "R1", "R2")))
          .members(
            VarDefBuilder.with( "member-0")
            .values(
              VarValueDefBuilder.with( "value-0")
              .properties( "P1")
              .build())
            .build(),
            VarDefBuilder.with( "member-1")
            .values(
              VarValueDefBuilder.with( "value-1")
              .properties( "P2", "P3")
              .build())
            .build())
          .build(),

          VarDefBuilder.with( "arg-0-1")
          .when( not( allOf( "P1")))
          .values(
            VarValueDefBuilder.with( "value-2")
            .type( ONCE)
            .when( not( not( "P2")))
            .build(),
            VarValueDefBuilder.with( "value-3")
            .properties( "R1", "R2", "R3")
            .build(),
            VarValueDefBuilder.with( "value-4")
            .type( FAILURE)
            .build())
          .build())
        .build())
      .build();

    // When...
    SystemInputDef systemInputDef = systemInputResources_.read( "system-input-def-6.xml");

    // Then...
    assertMatches( "system-input-def-6.xml", expected, systemInputDef, Matchers.systemInputDefMatcher); 
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7.   getSystemInputDef (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> None </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> No </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> AnyOf </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> None </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_7()
    {
    // Given...
    SystemInputDef expected =
      SystemInputDefBuilder.with( "System-7")
      .functions(
        FunctionInputDefBuilder.with( "Function-0")
        .vars(
          "arg",
          VarDefBuilder.with( "arg-0-0")
          .has( "A0", "AV0")
          .has( "A1", "AV1")
          .values(
            VarValueDefBuilder.with( "value-0")
            .properties( "P1", "P3", "P4", "P5", "P6", "P7")
            .when( allOf( "R1"))
            .build())
          .build())

        .vars(
          "env",
          VarSetBuilder.with( "env-0-0")
          .has( "A3", "NV3")
          .has( "A4", "AV4")
          .when( anyOf( "P1"))
          .members(
            VarDefBuilder.with( "member-0")
            .values(
              VarValueDefBuilder.with( "value-1")
              .properties( "R1")
              .build())
            .build())
          .build())
        
        .build())
      .build();

    // When...
    SystemInputDef systemInputDef = systemInputResources_.read( "system-input-def-7.xml");

    // Then...
    assertMatches( "system-input-def-7.xml", expected, systemInputDef, Matchers.systemInputDefMatcher); 
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8.   getSystemInputDef (Failure: System.Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD><FONT color=red> Undefined </FONT></TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> None </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> No </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_8()
    {
    assertException( "system-input-def-8.xml", 1, "No \"name\" attribute specified");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9.   getSystemInputDef (Failure: Function.Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD><FONT color=red> Undefined </FONT></TD></TR>
   * <TR><TD> Type.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_9()
    {
    assertException( "system-input-def-9.xml", 62, "No \"name\" attribute specified");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 10.  getSystemInputDef (Failure: Type.Count) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD><FONT color=red> None </FONT></TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> NA </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> NA </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> NA </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_10()
    {
    assertException( "system-input-def-10.xml", 3, "No input variables defined for FunctionInputDef[Function-0]");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 12.  getSystemInputDef (Failure: Var.Count) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD><FONT color=red> Undefined </FONT></TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> AnyOf </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> AnyOf </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> None </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_12()
    {
    assertException( "system-input-def-12.xml", 7, "No input variables defined for FunctionInputDef[Function-0]");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 13.  getSystemInputDef (Failure: Var.Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD><FONT color=red> Undefined </FONT></TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> Many </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_13()
    {
    assertException( "system-input-def-13.xml", 24, "No \"name\" attribute specified");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 14.  getSystemInputDef (Failure: Var.Condition) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD><FONT color=red> Redefined </FONT></TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> None </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> No </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_14()
    {
    assertException( "system-input-def-14.xml", 14, "Condition already defined for this element");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 15.  getSystemInputDef (Failure: Var.Condition) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD><FONT color=red> Many </FONT></TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> AnyOf </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> None </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_15()
    {
    assertException( "system-input-def-15.xml", 33, "Condition already defined for this element");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 16.  getSystemInputDef (Failure: Var.ValidValues) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD><FONT color=red> None </FONT></TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> Many </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_16()
    {
    assertException( "system-input-def-16.xml", 23, "No valid values defined for VarDef[env-0-0]");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 17.  getSystemInputDef (Failure: Value.Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD><FONT color=red> Undefined </FONT></TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> One </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_17()
    {
    assertException( "system-input-def-17.xml", 35, "No \"name\" attribute specified");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 18.  getSystemInputDef (Failure: Value.Condition) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> None </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> No </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD><FONT color=red> Redefined </FONT></TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_18()
    {
    assertException( "system-input-def-18.xml", 16, "Condition already defined for this element");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 19.  getSystemInputDef (Failure: Value.Condition) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> AnyOf </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD><FONT color=red> Many </FONT></TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> None </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_19()
    {
    assertException( "system-input-def-19.xml", 32, "Condition already defined for this element");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 20.  getSystemInputDef (Failure: Value.Properties) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD><FONT color=red> ForFailureValue </FONT></TD></TR>
   * <TR><TD> Property.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> Many </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_20()
    {
    assertException( "system-input-def-20.xml", 14, "Can't define properties for a failure value");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 21.  getSystemInputDef (Failure: Property.Count) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Count </TD> <TD><FONT color=red> ForFailureValue </FONT></TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> One </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_21()
    {
    assertException( "system-input-def-21.xml", 36, "Can't define properties for a failure value");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 22.  getSystemInputDef (Failure: Property.Properties) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD><FONT color=red> None </FONT></TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> AnyOf </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> None </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_22()
    {
    assertException( "system-input-def-22-1.xml", 10, "No property names specified");
    assertException( "system-input-def-22-2.xml", 11, "No property names specified");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 23.  getSystemInputDef (Failure: VarSet.Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> None </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> No </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD><FONT color=red> Undefined </FONT></TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_23()
    {
    assertException( "system-input-def-23.xml", 19, "No \"name\" attribute specified");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 24.  getSystemInputDef (Failure: VarSet.Condition) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> Many </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> One </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD><FONT color=red> Redefined </FONT></TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_24()
    {
    assertException( "system-input-def-24.xml", 39, "Condition already defined for this element");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 25.  getSystemInputDef (Failure: VarSet.Condition) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> NA </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD><FONT color=red> Many </FONT></TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> Many </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_25()
    {
    assertException( "system-input-def-25.xml", 76, "Condition already defined for this element");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 26.  getSystemInputDef (Failure: VarSet.Members) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD><FONT color=red> None </FONT></TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> One </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_26()
    {
    assertException( "system-input-def-26.xml", 11, "No members defined for VarSet[arg-0-0]");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 27.  getSystemInputDef (Failure: Condition.Members) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> Not </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> None </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD><FONT color=red> Undefined </FONT></TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_27()
    {
    assertException( "system-input-def-27-1.xml", 22, "No member defined for this Not condition");
    assertException( "system-input-def-27-2.xml", 26, "No member defined for this When condition");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 28.  getSystemInputDef (Failure: Invalid member) </TH></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_28()
    {
    assertException( "system-input-def-28.xml", 22, "The AllOf element is not allowed at this location");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 29.  getSystemInputDef (Failure: Value.Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> <FONT color=red> Duplicate </FONT> </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> AnyOf </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> None </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_29()
    {
    assertException( "system-input-def-29.xml", 23, "Value=value-1 already defined for variable=member-0");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 30.  getSystemInputDef (Failure: Function.Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> <FONT color=red> Duplicate </FONT> </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> AnyOf </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> None </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_30()
    {
    assertException( "system-input-def-30.xml", 39, "Function=Function-0 already defined for system=System-7");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 31.  getSystemInputDef (Failure: Var.Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> <FONT color=red> Duplicate </FONT> </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> AnyOf </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> None </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_31()
    {
    assertException( "system-input-def-31.xml", 27, "Variable=env-0-0 already defined for function=Function-0");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 32.  getSystemInputDef (Failure: VarSet.Members) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> System.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Function.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Function.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Type.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Type.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Var.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Var.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Var.Condition </TD> <TD> None </TD></TR>
   * <TR><TD> Var.ValidValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.FailureValues </TD> <TD> One </TD></TR>
   * <TR><TD> Var.OnceValue </TD> <TD> Yes </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> Value.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> Value.Condition </TD> <TD> AllOf </TD></TR>
   * <TR><TD> Value.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Property.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> Property.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> VarSet.Count </TD> <TD> Many </TD></TR>
   * <TR><TD> VarSet.Name </TD> <TD> Defined </TD></TR>
   * <TR><TD> VarSet.WhenProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.WhenNotProperties </TD> <TD> None </TD></TR>
   * <TR><TD> VarSet.Condition </TD> <TD> AnyOf </TD></TR>
   * <TR><TD> VarSet.Members </TD> <TD> <FONT color=red> Duplicate </FONT> </TD></TR>
   * <TR><TD> Condition.Properties </TD> <TD> One </TD></TR>
   * <TR><TD> Condition.Members </TD> <TD> None </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_32()
    {
    assertException( "system-input-def-32.xml", 26, "Member=member-0 already defined for varSet=env-0-0");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 33.  getSystemInputDef (Failure: Unknown element) </TH></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_33()
    {
    assertException( "system-input-def-33.xml", 17, "Unknown element: Vars");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 34.  getSystemInputDef (Failure: System.Name, Invalid identifier) </TH></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_34()
    {
    assertException( "system-input-def-34.xml", 1, "Invalid \"name\" attribute: \"System 1\" is not a valid identifier");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 35.  getSystemInputDef (Failure: Function.Name, Invalid identifier) </TH></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_35()
    {
    assertException( "system-input-def-35.xml", 32, "Invalid \"name\" attribute: \"Function-1.Is.Invalid\" is not a valid identifier");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 36.  getSystemInputDef (Failure: Var.Name, Invalid identifier) </TH></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_36()
    {
    assertException( "system-input-def-36.xml", 18, "Invalid \"name\" attribute: \"env-0=0\" is not a valid identifier");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 37.  getSystemInputDef (Failure: Value.Name, Invalid identifier) </TH></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_37()
    {
    assertException( "system-input-def-37.xml", 55, "No \"name\" attribute specified");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 38.  getSystemInputDef (Failure: Var.WhenProperties, undefined property) </TH></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_38()
    {
    assertException( "system-input-def-38.xml", 13, "Property=XXX is undefined");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 39.  getSystemInputDef (Failure: Value.WhenNotProperties, undefined property) </TH></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_39()
    {
    assertException( "system-input-def-39.xml", 7, "Property=XXX is undefined");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 40.  getSystemInputDef (Failure: Value.Condition, undefined property) </TH></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_40()
    {
    assertException( "system-input-def-40.xml", 19, "Property=XXX is undefined");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 41.  getSystemInputDef (Failure: Invalid attribute) </TH></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_41()
    {
    assertException( "system-input-def-41.xml", 23, "Attribute=type is not allowed for Value elements");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 42.  getSystemInputDef (Failure: Duplicate annotation) </TH></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_42()
    {
    assertException( "system-input-def-42.xml", 74, "Can't add annotation: Annotation=A3 already set to 'AV3'");
    }

  /**
   * Tests {@link SystemInputDocReader#getSystemInputDef getSystemInputDef()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 43.   getSystemInputDef (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Value.Name </TD> <TD> Empty </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Blank </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Non-alphanumeric </TD></TR>
   * <TR><TD> Value.Name </TD> <TD> Non-ASCII </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testGetSystemInputDef_43()
    {
    // Given...
    SystemInputDef expected =
      SystemInputDefBuilder.with( "System-43")
      .functions(
        FunctionInputDefBuilder.with( "Function-0")
        .vars(
          "env",
          VarDefBuilder.with( "env-0-0")
          .values(
            VarValueDefBuilder.with( "").build(),
            VarValueDefBuilder.with( " ").build(),
            VarValueDefBuilder.with( "大丈夫").build(),
            VarValueDefBuilder.with( "\"<>").build(),
            VarValueDefBuilder.with( "!#+-)[{").build())
          .build())
        .build())
      .build();

    // When...
    SystemInputDef systemInputDef = systemInputResources_.read( "system-input-def-43.xml");

    // Then...
    assertMatches( "system-input-def-43.xml", expected, systemInputDef, Matchers.systemInputDefMatcher); 
    }

  
  @Test
  public void testGetSystemInputDef_valueObjects()
    {
    // Given...
    SystemInputDef expected =
      SystemInputDefBuilder.with( "System-Objects")
      .functions(
        FunctionInputDefBuilder.with( "Function-Objects")
        .vars(
          VarDefBuilder.with( "Var-Objects")
          .values(
            VarValueDefBuilder.with( Integer.MIN_VALUE).build(),
            VarValueDefBuilder.with( Boolean.TRUE).build(),
            VarValueDefBuilder.with( (Object) null).type( FAILURE).build(),
            VarValueDefBuilder.with( Long.MAX_VALUE).build(),
            VarValueDefBuilder.with( new BigDecimal( "0.12345")).build(),
            VarValueDefBuilder.with( new BigDecimal( "12345.0")).build())
          .build())
        .build())
      .build();

    // When...
    SystemInputDef systemInputDef = systemInputResources_.read( "system-input-def-objects.xml");

    // Then...
    assertMatches( "system-input-def-objects.xml", expected, systemInputDef, Matchers.systemInputDefMatcher); 
    }

  /**
   * Reports a failure if reading the given resource does <U>not</U> cause the expected exception at the expected location.
   */
  private void assertException( String resource, int expectedLine, String expectedMsg)
    {
    Throwable failure = null;
    try
      {
      systemInputResources_.read( resource);
      }
    catch( Throwable t)
      {
      failure = t;
      }

    if( failure == null)
      {
      fail( "Expected exception not thrown.");
      }
    
    Throwable cause;
    for( cause = failure.getCause();
         !(cause == null || cause instanceof SAXParseException);
         cause = cause.getCause());

    if( cause == null)
      {
      throw new RuntimeException( "Unexpected exception thrown", failure);
      }

    SAXParseException spe = (SAXParseException) cause;
    assertEquals( "Exception line", expectedLine, spe.getLineNumber());

    String actualMsg = spe.getException()==null? spe.getMessage() : spe.getException().getMessage();
    assertEquals( "Exception message", expectedMsg, actualMsg);
    }

  private SystemInputResources systemInputResources_ = new SystemInputResources( TestSystemInputDocReader.class);
  }
