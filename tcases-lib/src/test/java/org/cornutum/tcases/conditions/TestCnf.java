//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.conditions;

import org.cornutum.tcases.PropertySet;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import org.apache.commons.collections4.IteratorUtils;

/**
 * Runs tests for {@link Cnf} methods.
 *
 */
public class TestCnf
  {
  @Test
  public void convertAnyOf()
    {
    // Given...
    ICondition condition =
      new AnyOf()
      .add( new AllOf().add( new ContainsAll( "A")).add( new ContainsAll( "B")))
      .add( new ContainsAll( "C"));
      
    // When...
    IConjunct conjunct = Cnf.convert( condition);
    
    // Then...
    IDisjunct[] disjuncts = IteratorUtils.toArray( conjunct.getDisjuncts(), IDisjunct.class);
    assertThat( "Disjuncts", disjuncts.length, is( 2));
    assertThat( "Satisfied:  A,  B,  C", conjunct.satisfied( new PropertySet( "A", "B", "C")), is( true));
    assertThat( "Satisfied:  A,  B, !C", conjunct.satisfied( new PropertySet( "A", "B")), is( true));
    assertThat( "Satisfied:  A, !B,  C", conjunct.satisfied( new PropertySet( "A", "C")), is( true));
    assertThat( "Satisfied:  A, !B, !C", conjunct.satisfied( new PropertySet( "A")), is( false));
    assertThat( "Satisfied: !A,  B,  C", conjunct.satisfied( new PropertySet( "B", "C")), is( true));
    assertThat( "Satisfied: !A, !B,  C", conjunct.satisfied( new PropertySet( "C")), is( true));
    assertThat( "Satisfied: !A,  B, !C", conjunct.satisfied( new PropertySet( "B")), is( false));
    assertThat( "Satisfied: !A, !B, !C", conjunct.satisfied( new PropertySet()), is( false));

    // Given...
    condition = new Not( condition);
      
    // When...
    conjunct = Cnf.convert( condition);
    
    // Then...
    disjuncts = IteratorUtils.toArray( conjunct.getDisjuncts(), IDisjunct.class);
    assertThat( "Disjuncts", disjuncts.length, is( 2));
    assertThat( "Satisfied:  A,  B,  C", conjunct.satisfied( new PropertySet( "A", "B", "C")), is( false));
    assertThat( "Satisfied:  A,  B, !C", conjunct.satisfied( new PropertySet( "A", "B")), is( false));
    assertThat( "Satisfied:  A, !B,  C", conjunct.satisfied( new PropertySet( "A", "C")), is( false));
    assertThat( "Satisfied:  A, !B, !C", conjunct.satisfied( new PropertySet( "A")), is( true));
    assertThat( "Satisfied: !A,  B,  C", conjunct.satisfied( new PropertySet( "B", "C")), is( false));
    assertThat( "Satisfied: !A, !B,  C", conjunct.satisfied( new PropertySet( "C")), is( false));
    assertThat( "Satisfied: !A,  B, !C", conjunct.satisfied( new PropertySet( "B")), is( true));
    assertThat( "Satisfied: !A, !B, !C", conjunct.satisfied( new PropertySet()), is( true));
    }
  
  @Test
  public void convertNot()
    {
    // Given...
    ICondition condition =
      new Not()
      .add( new AnyOf().add( new ContainsAny( "A", "B")).add( new ContainsAny( "C")));
      
    // When...
    IConjunct conjunct = Cnf.convert( condition);
    
    // Then...
    IDisjunct[] disjuncts = IteratorUtils.toArray( conjunct.getDisjuncts(), IDisjunct.class);
    assertThat( "Disjuncts", disjuncts.length, is( 3));
    assertThat( "Satisfied:  A,  B,  C", conjunct.satisfied( new PropertySet( "A", "B", "C")), is( false));
    assertThat( "Satisfied:  A,  B, !C", conjunct.satisfied( new PropertySet( "A", "B")), is( false));
    assertThat( "Satisfied:  A, !B,  C", conjunct.satisfied( new PropertySet( "A", "C")), is( false));
    assertThat( "Satisfied:  A, !B, !C", conjunct.satisfied( new PropertySet( "A")), is( false));
    assertThat( "Satisfied: !A,  B,  C", conjunct.satisfied( new PropertySet( "B", "C")), is( false));
    assertThat( "Satisfied: !A, !B,  C", conjunct.satisfied( new PropertySet( "C")), is( false));
    assertThat( "Satisfied: !A,  B, !C", conjunct.satisfied( new PropertySet( "B")), is( false));
    assertThat( "Satisfied: !A, !B, !C", conjunct.satisfied( new PropertySet()), is( true));

    // Given...
    condition = new Not( condition);
      
    // When...
    conjunct = Cnf.convert( condition);
    
    // Then...
    disjuncts = IteratorUtils.toArray( conjunct.getDisjuncts(), IDisjunct.class);
    assertThat( "Disjuncts", disjuncts.length, is( 1));
    assertThat( "Satisfied:  A,  B,  C", conjunct.satisfied( new PropertySet( "A", "B", "C")), is( true));
    assertThat( "Satisfied:  A,  B, !C", conjunct.satisfied( new PropertySet( "A", "B")), is( true));
    assertThat( "Satisfied:  A, !B,  C", conjunct.satisfied( new PropertySet( "A", "C")), is( true));
    assertThat( "Satisfied:  A, !B, !C", conjunct.satisfied( new PropertySet( "A")), is( true));
    assertThat( "Satisfied: !A,  B,  C", conjunct.satisfied( new PropertySet( "B", "C")), is( true));
    assertThat( "Satisfied: !A, !B,  C", conjunct.satisfied( new PropertySet( "C")), is( true));
    assertThat( "Satisfied: !A,  B, !C", conjunct.satisfied( new PropertySet( "B")), is( true));
    assertThat( "Satisfied: !A, !B, !C", conjunct.satisfied( new PropertySet()), is( false));
    }
  
  @Test
  public void convertTautology()
    {
    // Given...
    ICondition condition =
      new AnyOf()
      .add( new AllOf()
            .add( new ContainsAll( "A"))
            .add( new AnyOf()
                  .add( new ContainsAny( "B"))
                  .add( new Not().add( new ContainsAny( "C")))))
      .add( new Not()
            .add( new AnyOf()
                  .add( new ContainsAll( "A"))
                  .add( new AllOf()
                        .add( new Not().add( new ContainsAll( "B")))
                        .add( new ContainsAny( "C")))));
      
    // When...
    IConjunct conjunct = Cnf.convert( condition);
    
    // Then...
    assertThat( "Satisfied:  A,  B,  C", conjunct.satisfied( new PropertySet( "A", "B", "C")), is( true));
    assertThat( "Satisfied:  A,  B, !C", conjunct.satisfied( new PropertySet( "A", "B")), is( true));
    assertThat( "Satisfied:  A, !B,  C", conjunct.satisfied( new PropertySet( "A", "C")), is( false));
    assertThat( "Satisfied:  A, !B, !C", conjunct.satisfied( new PropertySet( "A")), is( true));
    assertThat( "Satisfied: !A,  B,  C", conjunct.satisfied( new PropertySet( "B", "C")), is( true));
    assertThat( "Satisfied: !A, !B,  C", conjunct.satisfied( new PropertySet( "C")), is( false));
    assertThat( "Satisfied: !A,  B, !C", conjunct.satisfied( new PropertySet( "B")), is( true));
    assertThat( "Satisfied: !A, !B, !C", conjunct.satisfied( new PropertySet()), is( true));

    // Given...
    condition = new Not( condition);
      
    // When...
    conjunct = Cnf.convert( condition);
    
    // Then...
    assertThat( "Satisfied:  A,  B,  C", conjunct.satisfied( new PropertySet( "A", "B", "C")), is( false));
    assertThat( "Satisfied:  A,  B, !C", conjunct.satisfied( new PropertySet( "A", "B")), is( false));
    assertThat( "Satisfied:  A, !B,  C", conjunct.satisfied( new PropertySet( "A", "C")), is( true));
    assertThat( "Satisfied:  A, !B, !C", conjunct.satisfied( new PropertySet( "A")), is( false));
    assertThat( "Satisfied: !A,  B,  C", conjunct.satisfied( new PropertySet( "B", "C")), is( false));
    assertThat( "Satisfied: !A, !B,  C", conjunct.satisfied( new PropertySet( "C")), is( true));
    assertThat( "Satisfied: !A,  B, !C", conjunct.satisfied( new PropertySet( "B")), is( false));
    assertThat( "Satisfied: !A, !B, !C", conjunct.satisfied( new PropertySet()), is( false));
    }

  }

