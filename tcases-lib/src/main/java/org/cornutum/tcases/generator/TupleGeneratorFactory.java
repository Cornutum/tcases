//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2015, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

/**
 * Creates {@link TupleGenerator} instances
 *
 */
public class TupleGeneratorFactory implements ITestCaseGeneratorFactory
  {
  /**
   * Creates a new {@link TupleGenerator} instance. Default properties of the new generator may
   * be defined by the given <CODE>defaultGenerator</CODE>. If <CODE>defaultGenerator</CODE> is null,
   * no default properties are defined.
   */
  public ITestCaseGenerator newGenerator( ITestCaseGenerator defaultGenerator)
    {
    TupleGenerator defaultTupleGenerator =
      defaultGenerator != null && defaultGenerator.getClass().equals( TupleGenerator.class)
      ? (TupleGenerator) defaultGenerator
      : null;
    
    return
        defaultTupleGenerator==null
        ? new TupleGenerator()
        : defaultTupleGenerator.cloneOf();
    }
  }
