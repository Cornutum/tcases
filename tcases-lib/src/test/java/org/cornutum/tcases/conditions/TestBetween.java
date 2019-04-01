//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.conditions;

import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;

import org.junit.Test;

/**
 * Runs tests for {@link Between} methods.
 *
 */
public class TestBetween
  {
  @Test
  public void whenNotSameProperty()
    {
    // Given...
    BoundedAssertion min = new AssertMore( "A", 0);
    BoundedAssertion max = new AssertLess( "B", 10);

    // Then...
    expectFailure( IllegalArgumentException.class).when( () -> new Between( min, max));
    }
  
  @Test
  public void whenMinInvalid()
    {
    // Given...
    BoundedAssertion min = new AssertNotMore( "A", 1);
    BoundedAssertion max = new AssertLess( "A", 10);

    // Then...
    expectFailure( IllegalArgumentException.class).when( () -> new Between( min, max));
    }
  
  @Test
  public void whenMaxInvalid()
    {
    // Given...
    BoundedAssertion min = new AssertNotLess( "A", 1);
    BoundedAssertion max = new AssertMore( "A", 10);

    // Then...
    expectFailure( IllegalArgumentException.class).when( () -> new Between( min, max));
    }
  }

