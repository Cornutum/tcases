//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

/**
 * Reports a parameter style value that is incompatible with the parameter definition.
 * Returns valid style value that can used instead.
 */
public class InvalidStyleException extends OpenApiException
  {
  private static final long serialVersionUID = 4008150702650878556L;

  /**
   * Creates a new InvalidStyleException instance.
   */
  public InvalidStyleException( String reason, String validStyle)
    {
    super( reason);
    validStyle_ = validStyle;
    }

  /**
   * Returns valid parameter style value that can be substituted in the parameter definition.
   */
  public String getValidStyle()
    {
    return validStyle_;
    }

  private final String validStyle_;
  }
