//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.util.Notifier;

import org.slf4j.Logger;

/**
 * Reports conditions found when resolving a {@link DataValue}.
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
        @Override
        public void warn( String[] location, String reason) {}
        @Override
        public void error( String[] location, String reason, String resolution) {}
        @Override
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
   * Returns a {@link ResolverConditionNotifier} that throws an ResolverException for any warning or error.
   */
  public static ResolverConditionNotifier fail()
    {
    return
      new ResolverConditionNotifier()
        {
        @Override
        public void warn( String[] location, String reason)
          {
          throw new ResolverException( location, new ResolverException( reason));
          }
        
        @Override
        public void error( String[] location, String reason, String resolution)
          {
          throw new ResolverException( location, new ResolverException( reason));
          }

        @Override
        public String toString()
          {
          return "FAIL";
          }   
        };
    }
  }
