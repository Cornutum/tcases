//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.cornutum.tcases.util.Notifier;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Reports conditions found when creating a Tcases model from an OpenAPI model.
 */
public interface ModelConditionNotifier extends Notifier
  {
  /**
   * Returns a {@link Notifier} that logs all conditions, using the default {@link Logger}.
   */
  public static Notifier log()
    {
    return Notifier.log( LoggerFactory.getLogger( InputModeller.class));
    }

  /**
   * Returns a {@link Notifier} that throws an OpenApiException for any warning or error.
   */
  public static Notifier fail()
    {
    return
      new Notifier()
        {
        @Override
        public void warn( String[] location, String reason)
          {
          throw new OpenApiException( location, new OpenApiException( reason));
          }
        
        @Override
        public void error( String[] location, String reason, String resolution)
          {
          throw new OpenApiException( location, new OpenApiException( reason));
          }

        @Override
        public String toString()
          {
          return "FAIL";
          }   
        };
    }
  }
