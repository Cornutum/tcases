//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.util.Asserts.Matcher;
import static org.cornutum.tcases.util.Asserts.*;

import static org.junit.Assert.*;

import java.util.Iterator;

/**
 * A {@link Matcher} for {@link IAnnotated} objects.
 */
public class AnnotatedMatcher implements Matcher<IAnnotated>
  {
  /**
   * Reports a failure if the expected object does not match the actual object.
   */
  public void assertEqual( String label, IAnnotated expected, IAnnotated actual)
    {
    assertSetEquals( label + ", annotations", expected.getAnnotations(), actual.getAnnotations());
    for( Iterator<String> annotations = expected.getAnnotations(); annotations.hasNext(); )
      {
      String annotation = annotations.next();
      assertEquals( label + ", annotation=" + annotation, expected.getAnnotation( annotation), actual.getAnnotation( annotation));
      }
    }
  }
