//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.junit.Test;
import static org.junit.Assert.*;

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
    assertEquals( "Unknown start", null, var);

    // When...
    var = functionInputDef.findVarPath( "A.B.D");
    
    // Then...
    assertEquals( "Unknown middle", null, var);

    // When...
    var = functionInputDef.findVarPath( "A.B.C.D.E");
    
    // Then...
    assertEquals( "Unknown end", null, var);

    // When...
    var = functionInputDef.findVarPath( null);
    
    // Then...
    assertEquals( "Null", null, var);
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
    assertEquals( "Descendant", "A.B.C.D", var.getPathName());

    // Given...
    functionInputDefBuilder.varDefAtPath( "X");
    functionInputDef = functionInputDefBuilder.build();

    // When...
    var = functionInputDef.findVarPath( "X");
    
    // Then...
    assertEquals( "Top-level", "X", var.getPathName());
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
    assertEquals( "Descendant", "A.B.C", var.getPathName());

    // When...
    var = functionInputDef.findVarPath( "A");
    
    // Then...
    assertEquals( "Top-level", "A", var.getPathName());
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
    assertEquals( "Top-level VarSet", null, var);

    // When...
    var = functionInputDef.findVarDefPath( "A.B.C");
    
    // Then...
    assertEquals( "Ancestor VarSet", null, var);

    // When...
    var = functionInputDef.findVarDefPath( "A.B.C.E");
    
    // Then...
    assertEquals( "Unknown end", null, var);
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
    assertEquals( "Descendant", "A.B.C.D", var.getPathName());

    // Given...
    functionInputDefBuilder.varDefAtPath( "X");
    functionInputDef = functionInputDefBuilder.build();

    // When...
    var = functionInputDef.findVarDefPath( "X");
    
    // Then...
    assertEquals( "Top-level", true, var != null);
    }

  }
