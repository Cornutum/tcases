//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.Notifier;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Reports conditions found when resolving an API {@link RequestCase}.
 */
public interface ResolverConditionNotifier extends Notifier
  {
  /**
   * Returns a {@link ResolverConditionNotifier} that ignores all conditions.
   */
  public static ResolverConditionNotifier ignore()
    {
    return
      new ResolverConditionNotifier()
        {
        public void warn( String[] location, String reason) {}
        public void error( String[] location, String reason, String resolution) {}
        public String toString() {return "IGNORE";}
        };
    }

  /**
   * Returns a {@link ResolverConditionNotifier} that logs all conditions, using the given {@link Logger}.
   */
  public static ResolverConditionNotifier log( final Logger logger)
    {
    return
      new ResolverConditionNotifier()
        {
        public void warn( String[] location, String reason)
          {
          logger.warn( messageFor( location, reason, null));
          }
        
        public void error( String[] location, String reason, String resolution)
          {
          logger.error( messageFor( location, reason, resolution));
          }

        public String toString()
          {
          return "LOG";
          }
        };
    }

  /**
   * Returns a {@link ResolverConditionNotifier} that logs all conditions, using the default {@link Logger}.
   */
  public static ResolverConditionNotifier log()
    {
    return log( LoggerFactory.getLogger( RequestCaseResolver.class));
    }

  /**
   * Returns a {@link ResolverConditionNotifier} that throws an ResolverException for any warning or error.
   */
  public static ResolverConditionNotifier fail()
    {
    return
      new ResolverConditionNotifier()
        {
        public void warn( String[] location, String reason)
          {
          throw new ResolverException( location, new ResolverException( reason));
          }
        
        public void error( String[] location, String reason, String resolution)
          {
          throw new ResolverException( location, new ResolverException( reason));
          }

        public String toString()
          {
          return "FAIL";
          }   
        };
    }
  }
