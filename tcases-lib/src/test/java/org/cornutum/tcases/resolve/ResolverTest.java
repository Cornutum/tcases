//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.util.ConditionRecorder;
import org.cornutum.tcases.util.Notifier;

import org.cornutum.hamcrest.ExpectedFailure.Failable;
import static org.cornutum.hamcrest.Composites.*;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;
import static org.hamcrest.MatcherAssert.*;

import java.util.List;
import java.util.Optional;
import java.util.Random;
import java.util.stream.Stream;
import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.toList;

/**
 * Base class for tests to verify resolution of {@link ITestCaseDef test case definitions}.
 */
public abstract class ResolverTest
  {
  /**
   * Returns the {@link ResolverContext} used for this test.
   */
  protected ResolverContext getResolverContext()
    {
    return getResolverContext( TestCaseConditionNotifier.log());
    }
  
  /**
   * Returns the {@link ResolverContext} used for this test.
   */
  protected ResolverContext getResolverContext( Notifier notifier)
    {
    return ResolverContext.builder().random( getRandom()).notifier( notifier).build();
    }

  /**
   * Returns {@link ResolverContext} to record conditions notified.
   */
  protected ResolverContext withConditionRecorder()
    {
    conditionRecorder_.clear();
    return getResolverContext( getConditionRecorder());
    }

  protected ConditionRecorder getConditionRecorder()
    {
    return conditionRecorder_;
    }

  /**
   * Verifies that an ResolverException occurs when the given Failable is executed.
   */
  protected void assertResolverException( Failable failable, String... expected)
    {
    expectFailure( ResolverException.class)
      .when( failable)
      .then( failure -> {
        Stream.Builder<String> causes = Stream.builder();
        for( Throwable cause = failure; cause != null; cause = cause.getCause())
          {
          causes.add( cause.getMessage());
          }
          
        assertThat( "Causes", causes.build().collect( toList()), listsMembers( expected));
        });
    }
  
  protected void assertWarnings( String... warnings)
    {
    assertThat( "Warnings", getConditionRecorder().getWarnings(), listsMembers( warnings));
    assertThat( "Errors", getConditionRecorder().getErrors(), listsMembers( emptyList()));
    }

  protected void assertErrors( String... errors)
    {
    assertThat( "Errors", getConditionRecorder().getErrors(), listsMembers( errors));
    assertThat( "Warnings", getConditionRecorder().getWarnings(), listsMembers( emptyList()));
    }
  
  protected void assertConditions( List<String> warnings, List<String> errors)
    {
    assertThat( "Warnings", getConditionRecorder().getWarnings(), listsMembers( warnings));
    assertThat( "Errors", getConditionRecorder().getErrors(), listsMembers( errors));
    }
  
  protected void assertConditionsNone()
    {
    assertConditions( emptyList(), emptyList());
    }

  /**
   * Returns the Random seed for this test.
   */
  protected long getSeed()
    {
    return seed_;
    }
  
  /**
   * Returns the Random instance for this test.
   */
  protected Random getRandom()
    {
    return random_;
    }

  private ConditionRecorder conditionRecorder_ = new ConditionRecorder();
  private Random random_ = new Random( seed_);

  private static long seed_ =
    Optional.ofNullable( System.getProperty( "seed"))
    .map( Long::valueOf)
    .orElse( 6745393444958854970L);
  }
