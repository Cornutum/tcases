//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.generator.*;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.AbstractMap.SimpleEntry;
import java.util.Map;
import java.util.Optional;
import java.util.Random;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toMap;


/**
 * For a {@link SystemInputDef system input definition}, updates the associated {@link GeneratorSet test case generators}
 * to reduce the number of generated test cases.
 *
 */
public class Reducer
  {
  /**
   * Creates a new Reducer object.
   */
  public Reducer()
    {
    }

  /**
   * For a {@link SystemInputDef system input definition}, updates the given {@link GeneratorSet test case generators}
   * to reduce the number of generated test cases, using the given {@link ReducerOptions options}.
   * <P/>
   * The {@link ITestCaseGenerator generator} for each function specified by the given {@link ReducerOptions options}
   * is updated according the procedure defined {@link #reduce( FunctionInputDef, ITestCaseGenerator, FunctionTestDef, ReducerOptions) here}.
   */
  public static Optional<GeneratorSet> reduce( SystemInputDef inputDef, GeneratorSet genDef, SystemTestDef baseDef, ReducerOptions options)
    {
    // Create a new set of generators to be updated.
    GeneratorSet genDefNew = genDef.cloneOf();

    // Identify functions to reduce.
    String function = options.getFunction();
    Stream<FunctionInputDef> functionInputDefs;
    if( function == null)
      {
      functionInputDefs = toStream( inputDef.getFunctionInputDefs());
      }
    else if( inputDef.getFunctionInputDef( function) == null)
      {
      throw new RuntimeException( "Function=" + function + " is not defined");
      }
    else
      {
      functionInputDefs = Stream.of( inputDef.getFunctionInputDef( function));
      }
    
    // For each of the specified function(s), find a seed that generates minimum test cases
    Map<String,ITestCaseGenerator> generatorsNew = 
      functionInputDefs
      .map( functionInputDef ->
        new SimpleEntry<String,ITestCaseGenerator>(
          functionInputDef.getName(),
          reduce(
            functionInputDef,
            genDefNew.getGenerator( functionInputDef.getName()),
            baseDef == null? null : baseDef.getFunctionTestDef( functionInputDef.getName()),
            options)
          .orElse( null)))
      .filter( e -> e.getValue() != null)
      .collect( toMap( SimpleEntry::getKey, SimpleEntry::getValue));

    if( generatorsNew.isEmpty())
      {
      logger_.info( "Generator definitions not changed");
      return Optional.empty();
      }
    else
      {
      generatorsNew.forEach( (f, g) -> genDefNew.setGenerator( f, g));
      return Optional.of( genDefNew);
      }
    }

  /**
   * For a {@link SystemInputDef system input definition}, returns a set of {@link GeneratorSet test case generators}
   * that {@link #reduce( SystemInputDef, GeneratorSet, SystemTestDef, ReducerOptions) reduces}
   * the number of test cases normally generated, using default {@link ReducerOptions options}.
   * Returns <CODE>Optional.empty()</CODE> if test cases were not reduced.
   */
  public static Optional<GeneratorSet> reduce( SystemInputDef inputDef)
    {
    return reduce( inputDef, GeneratorSet.basicGenerator(), null, new ReducerOptions());
    }

  /**
   * For a {@link FunctionInputDef function input definition}, updates the given {@link ITestCaseGenerator test case generator}
   * to reduce the number of generated test cases, using the given {@link ReducerOptions options}.
   * <P/>
   * The reducing process operates as a sequence of "rounds". Each round consists of a series of test case generations executions
   * called "samples". Each sample uses a new random seed to generate test cases for the specified function in
   * an attempt to find a seed that produces the fewest test cases.
   * <P/>
   * If all samples in a round complete without reducing the current minimum test case count, the reducing process
   * terminates. Otherwise, as soon as a new minimum is reached, a new round begins. The number of samples in each
   * subsequent round is determined using the {@link ReducerOptions#setResampleFactor "resample factor"}.
   * <P/>
   * At the end of the reducing process, if test cases have been reduced, returns a updated {@link ITestCaseGenerator} with the
   * random seed value that produce the minimum test case count. Otherwise, returns <CODE>Optional.empty()</CODE>.
   */
  public static Optional<ITestCaseGenerator> reduce( FunctionInputDef inputDef, ITestCaseGenerator generator, FunctionTestDef baseDef, ReducerOptions options)
    {
    String function = inputDef.getName();

    ITestCaseGenerator generatorNew =
      generator == null
      ? new TupleGenerator()
      : generator.cloneOf();

    if( options.isNewSeed())
      {
      generatorNew.setRandomSeed( null);
      }

    logger_.info( "[{}] Initializing test cases to be reduced", function);
    int initialCount = getTestCaseCount( inputDef, generatorNew, baseDef);
    int samples;
    int round;
    int minCount;
    long minSeed;
    long eqSeed;
    boolean reducing;
    Random random;
    for( samples = options.getSamples(),
           round = 1,
           minCount = initialCount,
           minSeed = 0L,
           eqSeed = 0L,
           reducing = true,
           random = new Random();
         
         samples > 0
           && reducing;
         
         samples = (int) Math.floor( samples * ( 1 + options.getResampleFactor())),
           round++)
      {
      // Perform next round of samples.
      logger_.info( "[{}] Round {}: starting next {} samples", new Object[]{ function, round, samples});
      int roundCount;
      long roundSeed;
      int i;
      for( i = 0,
             roundCount = 0,
             roundSeed = 0;
           
           i < samples
             && (roundCount =
                 getTestCaseCount
                 ( inputDef,
                   generatorNew,
                   baseDef,
                   (roundSeed = (long) (random.nextDouble() * Long.MAX_VALUE))))
             >= minCount;
             
           eqSeed = roundCount==minCount? roundSeed : eqSeed,
             i++);

      reducing = i < samples;
      if( reducing)
        {
        logger_.info( "[{}] Round {}: after {} samples, reached {} test cases with seed={}", new Object[]{ function, round, i+1, roundCount, roundSeed});
        minCount = roundCount;
        minSeed = roundSeed;
        }
      else
        {
        logger_.info( "[{}] Round {}: after {} samples, terminating with {} test cases", new Object[]{ function, round, samples, minCount});
        }
      }

    Optional<ITestCaseGenerator> reduced;
    if( minCount < initialCount || (options.isNewSeed() && eqSeed != 0))
      {
      minSeed = minSeed==0? eqSeed : minSeed;
      logger_.info( "[{}] Reduced to {} test cases with seed={} -- updating generator definition", new Object[]{ function, minCount, minSeed});
      generatorNew.setRandomSeed( minSeed);
      reduced = Optional.of( generatorNew);
      }
    else
      {
      logger_.info( "[{}] Could not reduce initial {} test cases", new Object[]{ function, initialCount});
      reduced = Optional.empty();
      }

    return reduced;
    }

  /**
   * For a {@link FunctionInputDef function input definition}, returns a {@link ITestCaseGenerator test case generator}
   * that {@link #reduce( FunctionInputDef, ITestCaseGenerator, FunctionTestDef, ReducerOptions) reduces}
   * the number of test cases normally generated, using default {@link ReducerOptions options}.
   * Returns <CODE>Optional.empty()</CODE> if test cases were not reduced.
   */
  public static Optional<ITestCaseGenerator> reduce( FunctionInputDef inputDef)
    {
    return reduce( inputDef, new TupleGenerator(), null, new ReducerOptions());
    }

  /**
   * Returns the total number of test cases generated for the given function.
   */
  private static int getTestCaseCount( FunctionInputDef inputDef, ITestCaseGenerator generator, FunctionTestDef baseDef)
    {
    return (int) toStream( generator.getTests( inputDef, baseDef).getTestCases()).count();
    }


  /**
   * Returns the total number of test cases generated for the given function using the given seed.
   */
  private static int getTestCaseCount( FunctionInputDef inputDef, ITestCaseGenerator generator, FunctionTestDef baseDef, long seed)
    {
    generator.setRandomSeed( seed);
    return getTestCaseCount( inputDef, generator, baseDef);
    }

  private static final Logger logger_ = LoggerFactory.getLogger( Reducer.class);
  }
