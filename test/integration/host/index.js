describe("host element handling", () => {
    let container;
    beforeEach(() => {
       container = document.createElement("div");
       document.body.appendChild(container); 
    })
    afterEach(() => {
        // container.remove();
    });

    it("basic host element creation", () => {
        const element = document.createElement("test-components-host-base-index");
        container.appendChild(element);

        expect(element.shadowRoot.childNodes.length).toBe(1);
    })
});