//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.Characters;

import java.util.Set;

/**
 * Defines a set of media type values that can be used by a request.
 */
public class MediaTypeDomain extends AbstractStringDomain
  {
  /**
   * Creates a new MediaTypeDomain instance.
   */
  public MediaTypeDomain()
    {
    super( 256, Characters.ASCII);
    }

  /**
   * Returns a MediaTypeDomain that contains any media type.
   */
  public static MediaTypeDomain any()
    {
    return new MediaTypeDomain();
    }

  /**
   * Returns a MediaTypeDomain that contains any media type.
   */
  public static MediaTypeDomain except( Set<String> excludedMediaTypes)
    {
    MediaTypeDomain domain = new MediaTypeDomain();
    domain.setExcluded( excludedMediaTypes);
    return domain;
    }

  /**
   * Returns a new random string of the given length for this domain.
   */
  @Override
protected String newValue( ResolverContext context, int length)
    {
    return mediaTypes_[ context.getRandom().nextInt( mediaTypes_.length)];
    }

  private static final String[] mediaTypes_ = new String[] {
    "application/json",
    "application/xml",
    "application/x-www-form-urlencoded", 
    "text/plain",
    "text/xml"
  };
}
