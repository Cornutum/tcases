//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import static org.cornutum.tcases.util.CollectionUtils.toStream;

import org.cornutum.hamcrest.BaseCompositeMatcher;
import org.cornutum.hamcrest.Composites;

import java.util.Map.Entry;
import static java.util.stream.Collectors.toMap;

/**
 * A composite matcher for {@link Annotated} objects.
 */
public class AnnotatedMatcher extends BaseCompositeMatcher<IAnnotated>
  {
  /**
   * Creates a new AnnotatedMatcher instance.
   */
  public AnnotatedMatcher( IAnnotated expected)
    {
    super( expected);

    expectThat( valueOf( "annotations", this::getAnnotationMappings).matches( Composites::containsMembers));
    }

  /**
   * Returns the annotation name/value mappings for the given IAnnotated instance.
   */
  private Iterable<Entry<String,String>> getAnnotationMappings( IAnnotated annotated)
    {
    return
      toStream( annotated.getAnnotations())
      .collect( toMap( name -> name, name -> annotated.getAnnotation( name)))
      .entrySet();
    }
  }
