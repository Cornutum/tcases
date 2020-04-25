package org.cornutum.tcases.openapi.moco;


import org.junit.Test;

import com.github.dreamhead.moco.junit.MocoJunitRunner;
import static com.github.dreamhead.moco.Moco.*;
import org.junit.ClassRule;
import com.github.dreamhead.moco.HttpsCertificate;
import static com.github.dreamhead.moco.HttpsCertificate.certificate;
import com.github.dreamhead.moco.HttpsServer;
// Test case dependencies

public class RequestTestDef2Test {
    private static final HttpsCertificate DEFAULT_CERTIFICATE = certificate( file( "/Users/kerrykimbrough/repos/tcases/tcases-moco/target/test-classes/org/cornutum/tcases/openapi/moco/myCert.cks"), "kss!", "css!");

    private static HttpsServer mocoServer;

    static {
        mocoServer = httpsServer( 12306, DEFAULT_CERTIFICATE);
        mocoServer.response( "Your request has been received.");
    }

    @ClassRule
    public static MocoJunitRunner runner = MocoJunitRunner.httpsRunner( mocoServer);
    // Test case declarations

    @Test
    public void getPosts_0() {
        // Given...
        // When testServer=http://localhost:12306...
        // Then...
    }

    @Test
    public void getPosts_1() {
        // Given...
        // When testServer=http://localhost:12306...
        // Then...
    }

    @Test
    public void getPosts_2() {
        // Given...
        // When testServer=http://localhost:12306...
        // Then...
    }

    @Test
    public void getPosts_3() {
        // Given...
        // When testServer=http://localhost:12306...
        // Then...
    }

    @Test
    public void getPosts_4() {
        // Given...
        // When testServer=http://localhost:12306...
        // Then...
    }

    @Test
    public void getPosts_5() {
        // Given...
        // When testServer=http://localhost:12306...
        // Then...
    }

    @Test
    public void getPosts_6() {
        // Given...
        // When testServer=http://localhost:12306...
        // Then...
    }
    // Test case closing
}
