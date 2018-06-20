//////////////////////////////////////////////////////////////////////////////
//
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.annotation;

import org.cornutum.tcases.annotation.creator.OutputAnnotationContainer;

/**
 * Interface for testcases with methods to inject test metadata.
 */
public interface TestMetadataAware {
    /**
     *  will be called with the test Id.
     */
    void setTestMetadata(int id, boolean isFailure, OutputAnnotationContainer outputAnnotationContainer);
}
