//////////////////////////////////////////////////////////////////////////////
//
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.annotation;

import org.cornutum.tcases.annotation.creator.OutputAnnotationContainer;

/**
 * Default abstract implementation of TestMetadataAware
 */
public abstract class AbstractTestCase implements TestMetadataAware {

    private int testCaseId;

    private boolean isFailure;

    private OutputAnnotationContainer outputAnnotations;

    public void setTestMetadata(int id, boolean isFailure, OutputAnnotationContainer outputAnnotationContainer) {
        this.testCaseId = id;
        this.isFailure = isFailure;
        this.outputAnnotations = outputAnnotationContainer;
    }

    public int getTestCaseId() {
        return testCaseId;
    }

    public OutputAnnotationContainer having() {
        return outputAnnotations;
    }

    public boolean isFailure() {
        return isFailure;
    }
}
