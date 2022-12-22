//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.util.Notifier;

import org.slf4j.LoggerFactory;

/**
 * Reports conditions found when resolving a {@link org.cornutum.tcases.ITestCaseDef test case definition}.
 */
public interface TestCaseConditionNotifier extends Notifier
  {
  /**
   * Returns a {@link Notifier} that logs all conditions.
   */
  public static Notifier log()
    {
    return Notifier.log( LoggerFactory.getLogger( TestCaseSchemaResolver.class));
    }
  
  /**
   * Returns a {@link Notifier} that throws an ResolverException for any warning or error.
   */
  public static Notifier fail()
    {
    return
      new Notifier()
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
