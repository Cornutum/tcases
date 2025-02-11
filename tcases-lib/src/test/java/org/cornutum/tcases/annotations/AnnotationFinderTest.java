//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2025, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.annotations;

import org.junit.Test;
import org.junit.FixMethodOrder;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.runners.MethodSorters.NAME_ASCENDING;

import java.util.List;
import static java.util.stream.Collectors.toList;

/**
 * Runs tests for {@link AnnotationFinder}
 */
@FixMethodOrder( NAME_ASCENDING)
public class AnnotationFinderTest
  {
  @Test
  public void inPackage()
    {
    // Given...
    AnnotationFinder finder =
      new AnnotationFinder( FixMethodOrder.class)
      .inPackage( getClass().getPackage().getName())
      .inPackage( "org.hamcrest");

    // When..
    List<Class<?>> classes = finder.find().collect( toList());

    // Then...
    assertThat( "Found", classes.size(), is( 1));
    assertThat( "Annotated", classes.get(0), is( getClass()));
    }

  }
