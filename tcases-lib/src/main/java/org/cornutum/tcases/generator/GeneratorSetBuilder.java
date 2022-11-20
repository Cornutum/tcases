//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import java.util.Optional;

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
      Optional.ofNullable( generatorSet)
      .map( GeneratorSet::cloneOf)
      .orElse( new GeneratorSet());
    
    return this;
    }

  /**
   * Adds a generator to this set for the given function.
   */
  public GeneratorSetBuilder generator( String functionName, ITestCaseGenerator generator)
    {
    generatorSet_.addGenerator( functionName, generator);
    return this;
    }

  /**
   * Adds a default generator to this set.
   */
  public GeneratorSetBuilder defaultGenerator( ITestCaseGenerator generator)
    {
    return generator( null, generator);
    }

  GeneratorSet generatorSet_;
  }

