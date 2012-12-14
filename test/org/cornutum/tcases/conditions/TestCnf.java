//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.conditions;

import org.cornutum.tcases.PropertySet;

import org.junit.Test;
import static org.junit.Assert.*;

import org.apache.commons.collections15.IteratorUtils;

/**
 * Runs tests for {@link Cnf} methods.
 *
 * @version $Revision$, $Date$
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
    assertEquals( "Disjuncts", 2, disjuncts.length);
    assertEquals( "Satisfied:  A,  B,  C", true, conjunct.satisfied( new PropertySet( "A", "B", "C")));
    assertEquals( "Satisfied:  A,  B, !C", true, conjunct.satisfied( new PropertySet( "A", "B")));
    assertEquals( "Satisfied:  A, !B,  C", true, conjunct.satisfied( new PropertySet( "A", "C")));
    assertEquals( "Satisfied:  A, !B, !C", false, conjunct.satisfied( new PropertySet( "A")));
    assertEquals( "Satisfied: !A,  B,  C", true, conjunct.satisfied( new PropertySet( "B", "C")));
    assertEquals( "Satisfied: !A, !B,  C", true, conjunct.satisfied( new PropertySet( "C")));
    assertEquals( "Satisfied: !A,  B, !C", false, conjunct.satisfied( new PropertySet( "B")));
    assertEquals( "Satisfied: !A, !B, !C", false, conjunct.satisfied( new PropertySet()));

    // Given...
    condition = new Not( condition);
      
    // When...
    conjunct = Cnf.convert( condition);
    
    // Then...
    disjuncts = IteratorUtils.toArray( conjunct.getDisjuncts(), IDisjunct.class);
    assertEquals( "Disjuncts", 2, disjuncts.length);
    assertEquals( "Satisfied:  A,  B,  C", false, conjunct.satisfied( new PropertySet( "A", "B", "C")));
    assertEquals( "Satisfied:  A,  B, !C", false, conjunct.satisfied( new PropertySet( "A", "B")));
    assertEquals( "Satisfied:  A, !B,  C", false, conjunct.satisfied( new PropertySet( "A", "C")));
    assertEquals( "Satisfied:  A, !B, !C", true, conjunct.satisfied( new PropertySet( "A")));
    assertEquals( "Satisfied: !A,  B,  C", false, conjunct.satisfied( new PropertySet( "B", "C")));
    assertEquals( "Satisfied: !A, !B,  C", false, conjunct.satisfied( new PropertySet( "C")));
    assertEquals( "Satisfied: !A,  B, !C", true, conjunct.satisfied( new PropertySet( "B")));
    assertEquals( "Satisfied: !A, !B, !C", true, conjunct.satisfied( new PropertySet()));
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
    assertEquals( "Disjuncts", 3, disjuncts.length);
    assertEquals( "Satisfied:  A,  B,  C", false, conjunct.satisfied( new PropertySet( "A", "B", "C")));
    assertEquals( "Satisfied:  A,  B, !C", false, conjunct.satisfied( new PropertySet( "A", "B")));
    assertEquals( "Satisfied:  A, !B,  C", false, conjunct.satisfied( new PropertySet( "A", "C")));
    assertEquals( "Satisfied:  A, !B, !C", false, conjunct.satisfied( new PropertySet( "A")));
    assertEquals( "Satisfied: !A,  B,  C", false, conjunct.satisfied( new PropertySet( "B", "C")));
    assertEquals( "Satisfied: !A, !B,  C", false, conjunct.satisfied( new PropertySet( "C")));
    assertEquals( "Satisfied: !A,  B, !C", false, conjunct.satisfied( new PropertySet( "B")));
    assertEquals( "Satisfied: !A, !B, !C", true, conjunct.satisfied( new PropertySet()));

    // Given...
    condition = new Not( condition);
      
    // When...
    conjunct = Cnf.convert( condition);
    
    // Then...
    disjuncts = IteratorUtils.toArray( conjunct.getDisjuncts(), IDisjunct.class);
    assertEquals( "Disjuncts", 1, disjuncts.length);
    assertEquals( "Satisfied:  A,  B,  C", true, conjunct.satisfied( new PropertySet( "A", "B", "C")));
    assertEquals( "Satisfied:  A,  B, !C", true, conjunct.satisfied( new PropertySet( "A", "B")));
    assertEquals( "Satisfied:  A, !B,  C", true, conjunct.satisfied( new PropertySet( "A", "C")));
    assertEquals( "Satisfied:  A, !B, !C", true, conjunct.satisfied( new PropertySet( "A")));
    assertEquals( "Satisfied: !A,  B,  C", true, conjunct.satisfied( new PropertySet( "B", "C")));
    assertEquals( "Satisfied: !A, !B,  C", true, conjunct.satisfied( new PropertySet( "C")));
    assertEquals( "Satisfied: !A,  B, !C", true, conjunct.satisfied( new PropertySet( "B")));
    assertEquals( "Satisfied: !A, !B, !C", false, conjunct.satisfied( new PropertySet()));
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
    assertEquals( "Satisfied:  A,  B,  C", true, conjunct.satisfied( new PropertySet( "A", "B", "C")));
    assertEquals( "Satisfied:  A,  B, !C", true, conjunct.satisfied( new PropertySet( "A", "B")));
    assertEquals( "Satisfied:  A, !B,  C", false, conjunct.satisfied( new PropertySet( "A", "C")));
    assertEquals( "Satisfied:  A, !B, !C", true, conjunct.satisfied( new PropertySet( "A")));
    assertEquals( "Satisfied: !A,  B,  C", true, conjunct.satisfied( new PropertySet( "B", "C")));
    assertEquals( "Satisfied: !A, !B,  C", false, conjunct.satisfied( new PropertySet( "C")));
    assertEquals( "Satisfied: !A,  B, !C", true, conjunct.satisfied( new PropertySet( "B")));
    assertEquals( "Satisfied: !A, !B, !C", true, conjunct.satisfied( new PropertySet()));

    // Given...
    condition = new Not( condition);
      
    // When...
    conjunct = Cnf.convert( condition);
    
    // Then...
    assertEquals( "Satisfied:  A,  B,  C", false, conjunct.satisfied( new PropertySet( "A", "B", "C")));
    assertEquals( "Satisfied:  A,  B, !C", false, conjunct.satisfied( new PropertySet( "A", "B")));
    assertEquals( "Satisfied:  A, !B,  C", true, conjunct.satisfied( new PropertySet( "A", "C")));
    assertEquals( "Satisfied:  A, !B, !C", false, conjunct.satisfied( new PropertySet( "A")));
    assertEquals( "Satisfied: !A,  B,  C", false, conjunct.satisfied( new PropertySet( "B", "C")));
    assertEquals( "Satisfied: !A, !B,  C", true, conjunct.satisfied( new PropertySet( "C")));
    assertEquals( "Satisfied: !A,  B, !C", false, conjunct.satisfied( new PropertySet( "B")));
    assertEquals( "Satisfied: !A, !B, !C", false, conjunct.satisfied( new PropertySet()));
    }

  }

