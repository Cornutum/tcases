//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

/**
 * Runs tests for {@link FunctionInputDef}.
 *
 */
public class TestFunctionInputDef
  {
  @Test
  public void findVarPath_Unknown()
    {
    // Given...
    FunctionInputDef functionInputDef =
      new FunctionInputDefBuilder()
      .vars(
        VarSetBuilder.with( "A")
        .members(
          VarSetBuilder.with( "B")
          .members(
            VarSetBuilder.with( "C")
            .members(
              VarDefBuilder.with( "D")
              .build())
            .build())
          .build())
        .build())
      .build();
    
    IVarDef var;

    // When...
    var = functionInputDef.findVarPath( "X");
    
    // Then...
    assertThat( "Unknown start", var, is( nullValue()));

    // When...
    var = functionInputDef.findVarPath( "A.B.D");
    
    // Then...
    assertThat( "Unknown middle", var, is( nullValue()));

    // When...
    var = functionInputDef.findVarPath( "A.B.C.D.E");
    
    // Then...
    assertThat( "Unknown end", var, is( nullValue()));

    // When...
    var = functionInputDef.findVarPath( null);
    
    // Then...
    assertThat( "Null", var, is( nullValue()));
    }

  @Test
  public void findVarPath_VarDef()
    {
    // Given...
    FunctionInputDef functionInputDef =
      new FunctionInputDefBuilder()
      .vars(
        VarSetBuilder.with( "A")
        .members(
          VarSetBuilder.with( "B")
          .members(
            VarSetBuilder.with( "C")
            .members(
              VarDefBuilder.with( "D")
              .build())
            .build())
          .build())
        .build())
      .build();

    IVarDef var;

    // When...
    var = functionInputDef.findVarPath( "A.B.C.D");
    
    // Then...
    assertThat( "Descendant", var.getPathName(), is( "A.B.C.D"));

    // Given...
    functionInputDef.addVarDef( VarDefBuilder.with( "X").build());

    // When...
    var = functionInputDef.findVarPath( "X");
    
    // Then...
    assertThat( "Top-level", var.getPathName(), is( "X"));
    }

  @Test
  public void findVarPath_VarSet()
    {
    // Given...
    FunctionInputDef functionInputDef =
      new FunctionInputDefBuilder()
      .vars(
        VarSetBuilder.with( "A")
        .members(
          VarSetBuilder.with( "B")
          .members(
            VarSetBuilder.with( "C")
            .members(
              VarDefBuilder.with( "D")
              .build())
            .build())
          .build())
        .build())
      .build();

    IVarDef var;

    // When...
    var = functionInputDef.findVarPath( "A.B.C");
    
    // Then...
    assertThat( "Descendant", var.getPathName(), is( "A.B.C"));

    // When...
    var = functionInputDef.findVarPath( "A");
    
    // Then...
    assertThat( "Top-level", var.getPathName(), is( "A"));
    }
  
  @Test
  public void findVarDefPath_Unknown()
    {
    // Given...
    FunctionInputDef functionInputDef =
      new FunctionInputDefBuilder()
      .vars(
        VarSetBuilder.with( "A")
        .members(
          VarSetBuilder.with( "B")
          .members(
            VarSetBuilder.with( "C")
            .members(
              VarDefBuilder.with( "D")
              .build())
            .build())
          .build())
        .build())
      .build();

    VarDef var;

    // When...
    var = functionInputDef.findVarDefPath( "A");
    
    // Then...
    assertThat( "Top-level VarSet", var, is( nullValue()));

    // When...
    var = functionInputDef.findVarDefPath( "A.B.C");
    
    // Then...
    assertThat( "Ancestor VarSet", var, is( nullValue()));

    // When...
    var = functionInputDef.findVarDefPath( "A.B.C.E");
    
    // Then...
    assertThat( "Unknown end", var, is( nullValue()));
    }

  @Test
  public void findVarDefPath_VarDef()
    {
    // Given...
    FunctionInputDef functionInputDef =
      new FunctionInputDefBuilder()
      .vars(
        VarSetBuilder.with( "A")
        .members(
          VarSetBuilder.with( "B")
          .members(
            VarSetBuilder.with( "C")
            .members(
              VarDefBuilder.with( "D")
              .build())
            .build())
          .build())
        .build())
      .build();

    VarDef var;

    // When...
    var = functionInputDef.findVarDefPath( "A.B.C.D");
    
    // Then...
    assertThat( "Descendant", var.getPathName(), is( "A.B.C.D"));

    // Given...
    functionInputDef.addVarDef( VarDefBuilder.with( "X").build());

    // When...
    var = functionInputDef.findVarDefPath( "X");
    
    // Then...
    assertThat( "Top-level", var != null, is( true));
    }

  }
