//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

/**
 * Provides a context for reporting model conditions found when processing an OpenAPI specification
 */
public class NotificationContext extends OpenApiContext
  {
  /**
   * Creates a new NotificationContext instance.
   */
  public NotificationContext()
    {
    this( null);
    }
  
  /**
   * Creates a new NotificationContext instance.
   */
  public NotificationContext( ModelConditionNotifier notifier)
    {
    notifier_ =
      notifier == null
      ? ModelConditionNotifier.fail()
      : notifier;
    }
  
  /**
   * Reports a condition in the OpenAPI model that will affect the expected Tcases model.
   *
   * @param reason  A description of the condition
   */
  public void warn( String reason)
    {
    notifier_.warn( getLocation(), reason);
    }

  /**
   * Reports an error in the OpenAPI model that would have resulted in an inconsistent or infeasible Tcases model.
   *
   * @param reason  A description of the problem
   * @param resolution  A description of how the problem was resolved
   */
  public void error( String reason, String resolution)
    {
    notifier_.error( getLocation(), reason, resolution);
    }

  private final ModelConditionNotifier notifier_;
  }
