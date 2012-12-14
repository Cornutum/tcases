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
 * @version $Revision$, $Date$
 */
public class TestFunctionInputDef
  {
  @Test
  public void findVarPath_Unknown()
    {
    // Given...
    FunctionInputDefBuilder functionInputDefBuilder = new FunctionInputDefBuilder();
    functionInputDefBuilder.varDef( "A.B.C.D");
    
    FunctionInputDef functionInputDef = functionInputDefBuilder.build();
    IVarDef var;

    // When...
    var = functionInputDef.findVarPath( "X");
    
    // Then...
    assertEquals( "Unknown start", true, var == null);

    // When...
    var = functionInputDef.findVarPath( "A.B.D");
    
    // Then...
    assertEquals( "Unknown middle", true, var == null);

    // When...
    var = functionInputDef.findVarPath( "A.B.C.D.E");
    
    // Then...
    assertEquals( "Unknown end", true, var == null);

    // When...
    var = functionInputDef.findVarPath( null);
    
    // Then...
    assertEquals( "Null", true, var == null);
    }

  @Test
  public void findVarPath_VarDef()
    {
    // Given...
    FunctionInputDefBuilder functionInputDefBuilder = new FunctionInputDefBuilder();
    functionInputDefBuilder.varDef( "A.B.C.D");
    
    FunctionInputDef functionInputDef = functionInputDefBuilder.build();
    IVarDef var;

    // When...
    var = functionInputDef.findVarPath( "A.B.C.D");
    
    // Then...
    assertEquals( "Descendant", true, var != null);

    // Given...
    functionInputDefBuilder.varDef( "X");
    functionInputDef = functionInputDefBuilder.build();

    // When...
    var = functionInputDef.findVarPath( "X");
    
    // Then...
    assertEquals( "Top-level", true, var != null);
    }

  @Test
  public void findVarPath_VarSet()
    {
    // Given...
    FunctionInputDefBuilder functionInputDefBuilder = new FunctionInputDefBuilder();
    functionInputDefBuilder.varDef( "A.B.C.D");
    
    FunctionInputDef functionInputDef = functionInputDefBuilder.build();
    IVarDef var;

    // When...
    var = functionInputDef.findVarPath( "A.B.C");
    
    // Then...
    assertEquals( "Descendant", true, var != null);

    // When...
    var = functionInputDef.findVarPath( "A");
    
    // Then...
    assertEquals( "Top-level", true, var != null);
    }
  
  @Test
  public void findVarDefPath_Unknown()
    {
    // Given...
    FunctionInputDefBuilder functionInputDefBuilder = new FunctionInputDefBuilder();
    functionInputDefBuilder.varDef( "A.B.C.D");
    
    FunctionInputDef functionInputDef = functionInputDefBuilder.build();
    VarDef var;

    // When...
    var = functionInputDef.findVarDefPath( "A");
    
    // Then...
    assertEquals( "Top-level VarSet", true, var == null);

    // When...
    var = functionInputDef.findVarDefPath( "A.B.C");
    
    // Then...
    assertEquals( "Ancestor VarSet", true, var == null);

    // When...
    var = functionInputDef.findVarDefPath( "A.B.C.E");
    
    // Then...
    assertEquals( "Unknown end", true, var == null);
    }

  @Test
  public void findVarDefPath_VarDef()
    {
    // Given...
    FunctionInputDefBuilder functionInputDefBuilder = new FunctionInputDefBuilder();
    functionInputDefBuilder.varDef( "A.B.C.D");
    
    FunctionInputDef functionInputDef = functionInputDefBuilder.build();
    VarDef var;

    // When...
    var = functionInputDef.findVarDefPath( "A.B.C.D");
    
    // Then...
    assertEquals( "Descendant", true, var != null);

    // Given...
    functionInputDefBuilder.varDef( "X");
    functionInputDef = functionInputDefBuilder.build();

    // When...
    var = functionInputDef.findVarDefPath( "X");
    
    // Then...
    assertEquals( "Top-level", true, var != null);
    }

  }
