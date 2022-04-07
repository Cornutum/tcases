//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

/**
 * Base class for TestCaseWriter implementations that validate API request results.
 */
public abstract class ValidatingTestCaseWriter extends TestCaseContentWriter
  {
  /**
   * Changes if generated test cases will validate API requests responses.
   */
  public void setValidateResponses( boolean validateResponses)
    {
    validateResponses_ = validateResponses;
    }

  /**
   * Returns if generated test cases will validate API requests responses.
   */
  public boolean validateResponses()
    {
    return validateResponses_;
    }

  private boolean validateResponses_;
  }
