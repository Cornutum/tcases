//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.slf4j.LoggerFactory;

/**
 * Reports conditions found when resolving a {@link org.cornutum.tcases.ITestCaseDef test case definition}.
 */
public interface TestCaseConditionNotifier extends ResolverConditionNotifier
  {
  /**
   * Returns a {@link ResolverConditionNotifier} that logs all conditions.
   */
  public static ResolverConditionNotifier log()
    {
    return ResolverConditionNotifier.log( LoggerFactory.getLogger( TestCaseSchemaResolver.class));
    }
  }
