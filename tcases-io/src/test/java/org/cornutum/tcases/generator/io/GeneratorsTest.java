//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2023, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator.io;

import org.cornutum.tcases.generator.Generators;
import org.cornutum.tcases.generator.IGeneratorSet;
import static org.cornutum.tcases.generator.Generators.*;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

/**
 * Runs tests for the builder interfaces in {@link Generators}
 */
public class GeneratorsTest
  {
  @Test
  public void buildExample()
    {
    // Given...
    IGeneratorSet expected = generatorSetResources_.readJson( "generator-example.json");
    
    // When...
    IGeneratorSet generators =
      generators()

      .generatingByDefault(
        tuples( 2)
        .seed( 123456789)
        .combining(
          tuplesOf( 3)
          .include( "V-0.**", "V-1")
          .exclude( "V-0.V-0-1")
          .using(
            once(
              whenVar( "V-0.V-0-2").is( "L-0-2_2"),
              whenVar( "V-0.V-0-3").is( "L-0-3_2"),
              whenVar( "V-0.V-0-0").is( "L-0-0_2")))))

      .generatingFor( "F", tuples().combining( tuplesOf( 2).include( "V-0.V-0-0", "V-1")))

      .build();

    // Then...
    assertThat( "Generators", generators, is( expected));
    }

  private GeneratorSetResources generatorSetResources_ = new GeneratorSetResources( TestGeneratorSetJson.class);
  }
