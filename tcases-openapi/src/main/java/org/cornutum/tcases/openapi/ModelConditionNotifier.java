//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Reports conditions found when creating a Tcases model from an OpenAPI model.
 */
public interface ModelConditionNotifier extends Notifier
  {
  /**
   * Returns a {@link ModelConditionNotifier} that ignores all conditions.
   */
  public static ModelConditionNotifier ignore()
    {
    return
      new ModelConditionNotifier()
        {
        @Override
        public void warn( String[] location, String reason) {}
        @Override
        public void error( String[] location, String reason, String resolution) {}
        @Override
        public String toString() {return "IGNORE";}
        };
    }

  /**
   * Returns a {@link ModelConditionNotifier} that logs all conditions, using the given {@link Logger}.
   */
  public static ModelConditionNotifier log( final Logger logger)
    {
    return
      new ModelConditionNotifier()
        {
        @Override
        public void warn( String[] location, String reason)
          {
          logger.warn( messageFor( location, reason, null));
          }
        
        @Override
        public void error( String[] location, String reason, String resolution)
          {
          logger.error( messageFor( location, reason, resolution));
          }

        @Override
        public String toString()
          {
          return "LOG";
          }
        };
    }

  /**
   * Returns a {@link ModelConditionNotifier} that logs all conditions, using the default {@link Logger}.
   */
  public static ModelConditionNotifier log()
    {
    return log( LoggerFactory.getLogger( InputModeller.class));
    }

  /**
   * Returns a {@link ModelConditionNotifier} that throws an OpenApiException for any warning or error.
   */
  public static ModelConditionNotifier fail()
    {
    return
      new ModelConditionNotifier()
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
