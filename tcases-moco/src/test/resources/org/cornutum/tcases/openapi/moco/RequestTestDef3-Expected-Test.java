package org.cornutum.tcases.openapi.moco;


import org.junit.Test;

import com.github.dreamhead.moco.junit.MocoJunitRunner;
import static com.github.dreamhead.moco.Moco.*;
import org.junit.Rule;
import com.github.dreamhead.moco.HttpServer;
// Test case dependencies

public class RequestTestDef3Test {

    private static HttpServer myServer;

    static {
        myServer = httpServer( 321);
    }

    @Rule
    public MocoJunitRunner runner = MocoJunitRunner.httpRunner( myServer);
    // Test case declarations

    @Test
    public void getPosts_0() {
        // Given...
        // When testServer=http://localhost:321...
        // Then...
    }

    @Test
    public void getPosts_1() {
        // Given...
        // When testServer=http://localhost:321...
        // Then...
    }

    @Test
    public void getPosts_2() {
        // Given...
        // When testServer=http://localhost:321...
        // Then...
    }

    @Test
    public void getPosts_3() {
        // Given...
        // When testServer=http://localhost:321...
        // Then...
    }

    @Test
    public void getPosts_4() {
        // Given...
        // When testServer=http://localhost:321...
        // Then...
    }

    @Test
    public void getPosts_5() {
        // Given...
        // When testServer=http://localhost:321...
        // Then...
    }

    @Test
    public void getPosts_6() {
        // Given...
        // When testServer=http://localhost:321...
        // Then...
    }
    // Test case closing
}
