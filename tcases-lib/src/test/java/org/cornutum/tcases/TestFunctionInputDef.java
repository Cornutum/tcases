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
    FunctionInputDefBuilder functionInputDefBuilder = new FunctionInputDefBuilder();
    functionInputDefBuilder.varDefAtPath( "A.B.C.D");
    
    FunctionInputDef functionInputDef = functionInputDefBuilder.build();
    IVarDef var;

    // When...
    var = functionInputDef.findVarPath( "X");
    
    // Then...
    assertThat( "Unknown start", var, is( (Object) null));

    // When...
    var = functionInputDef.findVarPath( "A.B.D");
    
    // Then...
    assertThat( "Unknown middle", var, is( (Object) null));

    // When...
    var = functionInputDef.findVarPath( "A.B.C.D.E");
    
    // Then...
    assertThat( "Unknown end", var, is( (Object) null));

    // When...
    var = functionInputDef.findVarPath( null);
    
    // Then...
    assertThat( "Null", var, is( (Object) null));
    }

  @Test
  public void findVarPath_VarDef()
    {
    // Given...
    FunctionInputDefBuilder functionInputDefBuilder = new FunctionInputDefBuilder();
    functionInputDefBuilder.varDefAtPath( "A.B.C.D");
    
    FunctionInputDef functionInputDef = functionInputDefBuilder.build();
    IVarDef var;

    // When...
    var = functionInputDef.findVarPath( "A.B.C.D");
    
    // Then...
    assertThat( "Descendant", var.getPathName(), is( "A.B.C.D"));

    // Given...
    functionInputDefBuilder.varDefAtPath( "X");
    functionInputDef = functionInputDefBuilder.build();

    // When...
    var = functionInputDef.findVarPath( "X");
    
    // Then...
    assertThat( "Top-level", var.getPathName(), is( "X"));
    }

  @Test
  public void findVarPath_VarSet()
    {
    // Given...
    FunctionInputDefBuilder functionInputDefBuilder = new FunctionInputDefBuilder();
    functionInputDefBuilder.varDefAtPath( "A.B.C.D");
    
    FunctionInputDef functionInputDef = functionInputDefBuilder.build();
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
    FunctionInputDefBuilder functionInputDefBuilder = new FunctionInputDefBuilder();
    functionInputDefBuilder.varDefAtPath( "A.B.C.D");
    
    FunctionInputDef functionInputDef = functionInputDefBuilder.build();
    VarDef var;

    // When...
    var = functionInputDef.findVarDefPath( "A");
    
    // Then...
    assertThat( "Top-level VarSet", var, is( (Object) null));

    // When...
    var = functionInputDef.findVarDefPath( "A.B.C");
    
    // Then...
    assertThat( "Ancestor VarSet", var, is( (Object) null));

    // When...
    var = functionInputDef.findVarDefPath( "A.B.C.E");
    
    // Then...
    assertThat( "Unknown end", var, is( (Object) null));
    }

  @Test
  public void findVarDefPath_VarDef()
    {
    // Given...
    FunctionInputDefBuilder functionInputDefBuilder = new FunctionInputDefBuilder();
    functionInputDefBuilder.varDefAtPath( "A.B.C.D");
    
    FunctionInputDef functionInputDef = functionInputDefBuilder.build();
    VarDef var;

    // When...
    var = functionInputDef.findVarDefPath( "A.B.C.D");
    
    // Then...
    assertThat( "Descendant", var.getPathName(), is( "A.B.C.D"));

    // Given...
    functionInputDefBuilder.varDefAtPath( "X");
    functionInputDef = functionInputDefBuilder.build();

    // When...
    var = functionInputDef.findVarDefPath( "X");
    
    // Then...
    assertThat( "Top-level", var != null, is( true));
    }

  }
