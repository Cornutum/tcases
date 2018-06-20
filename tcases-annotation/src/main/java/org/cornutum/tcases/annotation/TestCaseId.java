//////////////////////////////////////////////////////////////////////////////
//
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Marker annotation for an integer field to be set to the generated testcase id.
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface TestCaseId {

}
