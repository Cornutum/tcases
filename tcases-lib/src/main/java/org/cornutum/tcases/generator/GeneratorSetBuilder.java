//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

/**
 * Builds {@link GeneratorSet} instances.
 *
 */
public class GeneratorSetBuilder
  {  
  /**
   * Creates a new builder for the given GeneratorSet.
   */
  public static GeneratorSetBuilder with( GeneratorSet generatorSet)
    {
    return new GeneratorSetBuilder( generatorSet);
    }


  /**
   * Creates a new GeneratorSetBuilder object.
   */
  public GeneratorSetBuilder()
    {
    start();
    }

  /**
   * Creates a new GeneratorSetBuilder object.
   */
  public GeneratorSetBuilder( GeneratorSet generatorSet)
    {
    start( generatorSet);
    }

  /**
   * Returns the current {@link GeneratorSet}.
   */
  public GeneratorSet build()
    {
    return generatorSet_;
    }

  /**
   * Starts building a new generator set.
   */
  public GeneratorSetBuilder start()
    {
    return start( null);
    }

  /**
   * Starts building a new generator set.
   */
  public GeneratorSetBuilder start( GeneratorSet generatorSet)
    {
    generatorSet_ =
      generatorSet == null
      ? new GeneratorSet()
      : generatorSet;
    
    return this;
    }

  /**
   * Adds a generator to this set.
   */
  public GeneratorSetBuilder generator( String functionName, ITestCaseGenerator generator)
    {
    generatorSet_.addGenerator( functionName, generator);
    return this;
    }

  GeneratorSet generatorSet_;
  }

